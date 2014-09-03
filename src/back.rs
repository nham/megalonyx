use libsyn;
use front::{Terminal, AnyTerminal, TerminalString, PosLookahead, NegLookahead,
            Class, ZeroOrMore, OneOrMore, Optional, Seq, Alt, Nonterminal,
            Label, Expression, RuleAction};
use middle::Grammar;

use std::gc::Gc;

pub struct Compiler<'a> {
    cx: &'a libsyn::ExtCtxt<'a>,
}

impl<'a> Compiler<'a> {
    pub fn new(cx: &'a libsyn::ExtCtxt) -> Compiler<'a> {
        Compiler { cx: cx }
    }

    pub fn generate_parsers(&self, grammar: &Grammar) -> Vec<Gc<libsyn::Item>> {
        let mut rule_parsers = Vec::new();
        for (n, d) in grammar.rules.iter() {
            let (action_ty, action_expr) = match d.action {
                Some(a) => (a.ty, a.expr),
                None => (quote_ty!(&*self.cx, ()), quote_expr!(&*self.cx, ())),
            };

            rule_parsers.push(
                self.generate_parser(*n, action_ty, action_expr, &d.expr)
            );
        }

        rule_parsers
    }


    // Generate a parser for a rule
    pub fn generate_parser(
        &self,
        rule_name: libsyn::Ident,
        action_ty: Gc<libsyn::Ty>,
        action_expr: Gc<libsyn::Expr>,
        expr: &Expression,
    ) -> Gc<libsyn::Item> {
        let input_ident = libsyn::Ident::new(libsyn::intern("input"));
        let parser_contents = self.generate_parser_expr(expr, input_ident);

        let qi = quote_item!(self.cx,
            fn $rule_name<'a>($input_ident: &'a str)
            -> Result<ParseResult<'a, $action_ty>, String> {
                match $parser_contents {
                    Err(e) => Err(e),
                    Ok(s) => Ok(($action_expr, s)),
                }
            }
        );

        qi.unwrap()
    }


    // Generate parsing code for a single Megalonyx expression
    // This must be an expression, since generate_parser() will just wrap it up
    // in a function
    fn generate_parser_expr(&self, expr: &Expression, input_ident: libsyn::Ident)
    -> Gc<libsyn::Expr> {
        match *expr {
            Terminal(c) =>
                self.gen_terminal_parser(c, input_ident),

            AnyTerminal =>
                self.gen_anyterminal_parser(input_ident),

            TerminalString(ref s) =>
                self.gen_terminalstring_parser(s.as_slice(), input_ident),

            Nonterminal(n) =>
                self.gen_nonterminal_parser(n, input_ident),

            PosLookahead(ref e) =>
                self.gen_poslookahead_parser(&**e, input_ident),

            NegLookahead(ref e) =>
                self.gen_neglookahead_parser(&**e, input_ident),

            Class(ref s) =>
                self.gen_class_parser(s.as_slice(), input_ident),

            ZeroOrMore(ref e) =>
                self.gen_star_parser(&**e, input_ident),

            OneOrMore(ref e) =>
                self.gen_plus_parser(&**e, input_ident),

            Optional(ref e) =>
                self.gen_opt_parser(&**e, input_ident),

            Seq(ref v) => {
                if v.len() == 0 {
                    fail!("Can't interpret a sequence of zero length");
                } else {
                    self.gen_seq_parser(v.as_slice(), input_ident)
                }
            },
            Alt(ref v) => {
                if v.len() == 0 {
                    fail!("Can't interpret a sequence of zero length");
                } else {
                    self.gen_alt_parser(v.as_slice(), input_ident)
                }
            },
            Label(_, ref e) => {
                // FIXME: currently ignoring bindings
                self.generate_parser_expr(&**e, input_ident)
            },
            _ => fail!("Unimplemented"),
        }
    }


    fn gen_terminal_parser(&self, c: char, input_ident: libsyn::Ident)
    -> Gc<libsyn::Expr> {
        quote_expr!(self.cx,
            if $input_ident.len() > 0 {
                let cr = $input_ident.char_range_at(0);
                if cr.ch == $c {
                    Ok($input_ident.slice_from(cr.next))
                } else {
                    Err(format!("Could not match '{}': (saw '{}' instead)",
                                $c, cr.ch))
                }
            } else {
                Err(format!("Could not match '{}' (end of input)", $c))
            }
        )
    }

    fn gen_anyterminal_parser(&self, input_ident: libsyn::Ident)
    -> Gc<libsyn::Expr> {
        quote_expr!(self.cx,
            if $input_ident.len() > 0 {
                let cr = $input_ident.char_range_at(0);
                Ok($input_ident.slice_from(cr.next))
            } else {
                Err(format!("Could not match '.' (end of input)"))
            }
        )
    }


    fn gen_terminalstring_parser(&self, sl: &str, input_ident: libsyn::Ident)
    -> Gc<libsyn::Expr> {
        let n = sl.len();
        quote_expr!(self.cx,
            if $input_ident.len() >= $n {
                if $input_ident.starts_with($sl) {
                    Ok($input_ident.slice_from($n))
                } else {
                    Err(format!("Could not match '{}': (saw '{}' instead)",
                                $sl, $input_ident))
                }
            } else {
                Err(format!("Could not match '{}' (end of input)", $sl))
            }
        )
    }

    fn gen_class_parser(&self, sl: &str, input_ident: libsyn::Ident)
    -> Gc<libsyn::Expr> {
        quote_expr!(self.cx,
            if $input_ident.len() > 0 {
                let cr = $input_ident.char_range_at(0);
                if $sl.find(cr.ch).is_some() {
                    Ok($input_ident.slice_from(cr.next))
                } else {
                    Err(format!("Could not match '[{}]': (saw '{}' instead)",
                                $sl, cr.ch))
                }
            } else {
                Err(format!("Could not match '[{}]' (end of input)", $sl))
            }
        )
    }

    fn gen_nonterminal_parser(&self, n: libsyn::Ident, input_ident: libsyn::Ident)
    -> Gc<libsyn::Expr> {
        quote_expr!(self.cx,
            match $n($input_ident) {
                Err(e) => Err(e),
                Ok(s) => Ok(s.val1()),
            }
        )
    }

    fn gen_poslookahead_parser(&self, exp: &Expression, input_ident: libsyn::Ident)
    -> Gc<libsyn::Expr> {
        let parser = self.generate_parser_expr(exp, input_ident);
        quote_expr!(self.cx,
            {
                let res: Result<&'a str, String> = $parser;
                match res {
                    Ok(_) => Ok($input_ident),
                    Err(e) => Err(e),
                }
            }
        )
    }

    fn gen_neglookahead_parser(&self, exp: &Expression, input_ident: libsyn::Ident)
    -> Gc<libsyn::Expr> {
        let parser = self.generate_parser_expr(exp, input_ident);
        quote_expr!(self.cx,
            {
                let res: Result<&'a str, String> = $parser;
                match res {
                    Ok(_) => Err(format!("Could not match ! expression")),
                    Err(_) => Ok($input_ident),
                }
            }
        )
    }

    fn gen_star_parser(&self, exp: &Expression, input_ident: libsyn::Ident)
    -> Gc<libsyn::Expr> {
        let parser = self.generate_parser_expr(exp, input_ident);
        let new_fn_name = libsyn::gensym_ident("star");
        quote_expr!(self.cx,
            {
                fn $new_fn_name<'a>($input_ident: &'a str) -> Result<&'a str, String> {
                    $parser
                }

                let mut inp = $input_ident;
                loop {
                    match $new_fn_name(inp) {
                        Ok(rem) => inp = rem,
                        Err(_) => break,
                    }
                }
                Ok(inp)
            }
        )
    }

    fn gen_plus_parser(&self, exp: &Expression, input_ident: libsyn::Ident)
    -> Gc<libsyn::Expr> {
        let parser = self.generate_parser_expr(exp, input_ident);
        let new_fn_name = libsyn::gensym_ident("plus");
        quote_expr!(self.cx,
            {
                fn $new_fn_name<'a>($input_ident: &'a str) -> Result<&'a str, String> {
                    $parser
                }

                let mut inp = $input_ident;
                match $new_fn_name(inp) {
                    Err(e) => Err(e),
                    Ok(rem) => {
                        inp = rem;

                        loop {
                            match $new_fn_name(inp) {
                                Ok(rem) => inp = rem,
                                Err(_) => break,
                            }
                        }

                        Ok(inp)
                    },
                }
            }
        )
    }

    fn gen_opt_parser(&self, exp: &Expression, input_ident: libsyn::Ident)
    -> Gc<libsyn::Expr> {
        let parser = self.generate_parser_expr(exp, input_ident);
        quote_expr!(self.cx,
            match $parser {
                Ok(rem) => Ok(rem),
                Err(_) => Ok($input_ident),
            }
        )
    }


    fn gen_seq_parser(&self, exprs: &[Expression], input_ident: libsyn::Ident)
    -> Gc<libsyn::Expr> {
        if exprs.len() == 0 {
            fail!("Don't call gen_seq_parser with a slice of length 0")
        } else if exprs.len() == 1 {
            let parser = self.generate_parser_expr(&exprs[0], input_ident);
            quote_expr!(self.cx, $parser)
        } else {
            let parser = self.generate_parser_expr(&exprs[0], input_ident);
            let rem = libsyn::Ident::new(libsyn::intern("rem"));
            let parser2 = self.gen_seq_parser(exprs.slice_from(1), rem);
            quote_expr!(self.cx,
                match $parser {
                    Err(e) => Err(e),
                    Ok(rem) => $parser2,
                }
            )
        }
    }

    fn gen_alt_parser(&self, exprs: &[Expression], input_ident: libsyn::Ident)
    -> Gc<libsyn::Expr> {
        if exprs.len() == 0 {
            fail!("Don't call gen_alt_parser with a slice of length 0")
        } else if exprs.len() == 1 {
            let parser = self.generate_parser_expr(&exprs[0], input_ident);
            quote_expr!(self.cx, $parser)
        } else {
            let parser = self.generate_parser_expr(&exprs[0], input_ident);
            let parser2 = self.gen_alt_parser(exprs.slice_from(1), input_ident);
            quote_expr!(self.cx,
                match $parser {
                    Err(_) => $parser2,
                    Ok(rem) => Ok(rem),
                }
            )

        }
    }
}
