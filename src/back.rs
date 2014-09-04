use libsyn::{mod, ToSource};
use front::{Terminal, AnyTerminal, TerminalString, PosLookahead, NegLookahead,
            Class, ZeroOrMore, OneOrMore, Optional, Seq, Alt, Nonterminal,
            Label, Expression, RuleAction};
use middle::Grammar;

use std::gc::Gc;
use std::collections::HashMap;

fn new_parser<'a>(s: &str, sess: &'a libsyn::ParseSess) -> libsyn::Parser<'a> {
    libsyn::new_parser_from_source_str(sess, vec!(),
                                       "bogus".to_string(),
                                       s.to_string())
}


/*

other notes:

    letter = c:["aeiou"] n:num -> String {
        "hello".to_string() + c
    }

    nz_dig = ["123456789"]
    num = nz_dig ('0' / nz_dig)* / '0'

this seems like it should work. ('0' / nz_dig)* returning Vec<&str> seems super
awkward to me. should this be special cased?

also, should actual char literals and character classes return a char instead of
a str? and then maybe Vec<char> could be special cased to be an &str instead?

maybe "text" expressions (everything except non-terminals) should just return &str 
of consumed input. this seems to make the most sense to me and eliminates a LOT of 
problems. then we only have problems with non-terminals.

also is it just me or is sequence returning the last expression's value just
straight up wrong? if I have 

    nz_dig ('0' / nz_dig)*

then it would lose the initial character parsed. I definitely want that as the return value.


*/

pub struct Compiler<'a> {
    cx: &'a libsyn::ExtCtxt<'a>,
}

type Bindings = HashMap<libsyn::Ident, Gc<libsyn::Ty>>;

impl<'a> Compiler<'a> {
    pub fn new(cx: &'a libsyn::ExtCtxt) -> Compiler<'a> {
        Compiler { cx: cx }
    }

    // currently generates a module whose name is the grammar name
    // this module contains a function called 'parse', so you call
    // the parser by <grammar name>::parse(input)
    pub fn compile(&self, grammar: &Grammar) -> Gc<libsyn::Item> {
        let rule_parsers = self.generate_parsers(grammar);
        let grammar_name = grammar.name;
        let start_rule = grammar.start;

        // see below in qi for where 's' comes in to play
        let start_action = grammar.rules.find(&start_rule).unwrap().action;
        let (return_ty, ok_val) = match start_action {
            Some(a) => ( a.ty, quote_expr!(&*self.cx, s.val0()) ),
            None => ( quote_ty!(&*self.cx, &'a str),
                      quote_expr!(&*self.cx, s.val1()) ),
        };

        let qi = quote_item!(self.cx,
            mod $grammar_name {
                type ParseResult<'a, T> = (T, &'a str);

                pub fn parse<'a>(input: &'a str)
                -> Result<$return_ty, String> {
                    match $start_rule::parse(input) {
                        Err(e) => Err(e),
                        Ok(s) => Ok($ok_val),
                    }
                }

                $rule_parsers
            }
        );

        qi.unwrap()
    }

    fn generate_parsers(&self, grammar: &Grammar) -> Vec<Gc<libsyn::Item>> {
        let mut rule_parsers = Vec::new();
        for (n, d) in grammar.rules.iter() {
            rule_parsers.push(
                self.generate_parser(grammar, *n, d.action, &d.expr)
            );
        }

        rule_parsers
    }


    /* PROBLEMS WITH BINDINGS

       1) we won't be initializing everything at once, only one field at a time.
          but we're using a struct, so everything has to get a value at the same
          time. Solution: wrap everything in an Option?

       2) we can have shadowing of bindings, which is not really a problem except
          when the initial bindings have different types than the final bindings.
          in this case we have to be careful not to assign these initial bindings.
          I think it's safe to throw them away, they won't get use in the rule
          action (though in the full OMeta implementation I think actions can
          be anywhere, which is a huge problem?)

     */


    // Generate a parser for a rule
    fn generate_parser(
        &self,
        grammar: &Grammar,
        rule_name: libsyn::Ident,
        rule_action: Option<RuleAction>,
        expr: &Expression,
    ) -> Gc<libsyn::Item> {
        let input_ident = libsyn::Ident::new(libsyn::intern("input"));
        let parser_contents = self.generate_parser_expr(expr, input_ident, false);

        let mut map: Bindings = HashMap::new();
        self.find_bindings(expr, &mut map);

        let mut binding_lets = vec!();
        let mut binding_struct = None;
        let mut binding_impl = None;
        let mut binding_instance = None;

        if map.len() > 0 {
            let mut binding_struct_str = String::from_str("struct Binding<'a> {");

            let mut struct_init_str = String::from_str("Binding {");

            for (k, v) in map.iter() {
                binding_struct_str.push_str(
                    format!("{}: Option<{}>,", k.to_source(), v.to_source()
                ).as_slice());

                binding_lets.push(quote_stmt!(self.cx,
                                      let $k: $v = bindings.$k.unwrap();));

                struct_init_str.push_str(
                    format!("{}: None,", k.to_source()
                ).as_slice());
            }

            binding_struct_str.push_char('}');
            struct_init_str.push_char('}');

            let sess = libsyn::new_parse_sess();
            let mut p = new_parser(binding_struct_str.as_slice(), &sess);
            binding_struct = Some( p.parse_item_with_outer_attributes() );

            let sess = libsyn::new_parse_sess();
            let mut p = new_parser(struct_init_str.as_slice(), &sess);
            let struct_init = p.parse_expr();

            binding_impl = Some( quote_item!(self.cx,
                                     impl<'a> Binding<'a> {
                                         fn new() -> Binding<'a> {
                                             $struct_init
                                         }
                                     }
                                ));

            binding_instance = Some( quote_stmt!(self.cx,
                                 let mut bindings: Binding<'a> = Binding::new();
                               ));
        };

        let (rule_ty, rule_expr) =
            self.determine_rule_return(grammar, rule_action, expr);
        let qi =
            quote_item!(self.cx,
                mod $rule_name {
                    $binding_struct

                    $binding_impl

                    pub fn parse<'a>($input_ident: &'a str)
                    -> Result<super::ParseResult<'a, $rule_ty>, String> {
                        $binding_instance
                        match $parser_contents {
                            Err(e) => Err(e),
                            Ok((_val, s)) => {
                                $binding_lets

                                Ok(($rule_expr, s))
                            }
                        }
                    }
                }
            );

        qi.unwrap()
    }


    /*
        The parser expr return value is a bit tricky. If we just follow the paper
        we end up with something that is overly restrictive, because for example
        a star expression returns a vector, so "def"* / "abc" is not allowed
        since all arms of a choice need to have the same return type, but
        in this case one is (Vec<&str>, &str) and the other is (&str, &str).

        The other problem is that there are times when you actually do want a
        star to return a vector, but other times when you don't.

        The compromise I've come up with is to have everything return (&str, &str)
        except when its being bound.
    */
    fn determine_rule_return(
        &self,
        grammar: &Grammar,
        rule_action: Option<RuleAction>,
        expr: &Expression,
    ) -> (Gc<libsyn::Ty>, Gc<libsyn::Expr>) {
        match rule_action {
            None => {
                let rule_expr = quote_expr!(self.cx, _val);

                let rule_ty = match *expr {
                    PosLookahead(_) =>
                        quote_ty!(self.cx, ()),

                    NegLookahead(_) =>
                        quote_ty!(self.cx, ()),

                    ZeroOrMore(_) =>
                        quote_ty!(self.cx, Vec<&'a str>),

                    OneOrMore(_) =>
                        quote_ty!(self.cx, Vec<&'a str>),

                    Optional(_) =>
                        quote_ty!(self.cx, Option<&'a str>),

                    Nonterminal(id) => {
                        let rule_data = grammar.rules.find(&id).unwrap();
                        match rule_data.action {
                            None =>
                                quote_ty!(self.cx, &'a str),
                            Some(RuleAction { ty: ty, expr: _ }) =>
                                ty,
                        }
                    },
                    _ =>
                        quote_ty!(self.cx, &'a str),
                };

                (rule_ty, rule_expr)
            },
            Some(RuleAction { ty: ty, expr: expr }) => (ty, expr),
        }
    }


    // Generate parsing code for a single Megalonyx expression
    // This must be an expression, since generate_parser() will just wrap it up
    // in a function
    // The expression must evaluate to Result<(T, &'a str), String>
    // for some T.
    //
    // is_bound determines the value that a parsing expression returns. e.g.
    // star and plus operators return a vector of values if theyre being bound,
    // but just return consumed input otherwise
    fn generate_parser_expr(
        &self,
        expr: &Expression,
        input_ident: libsyn::Ident,
        is_bound: bool,
    ) -> Gc<libsyn::Expr> {
        match *expr {
            Terminal(c) =>
                self.gen_terminal_parser(c, input_ident),

            AnyTerminal =>
                self.gen_anyterminal_parser(input_ident),

            TerminalString(ref s) =>
                self.gen_terminalstring_parser(s.as_slice(), input_ident),

            Nonterminal(n) =>
                self.gen_nonterminal_parser(n, input_ident),

            Class(ref s) =>
                self.gen_class_parser(s.as_slice(), input_ident),

            PosLookahead(ref e) =>
                self.gen_poslookahead_parser(&**e, input_ident, is_bound),

            NegLookahead(ref e) =>
                self.gen_neglookahead_parser(&**e, input_ident, is_bound),

            ZeroOrMore(ref e) =>
                self.gen_star_parser(&**e, input_ident, is_bound),

            OneOrMore(ref e) =>
                self.gen_plus_parser(&**e, input_ident, is_bound),

            Optional(ref e) =>
                self.gen_opt_parser(&**e, input_ident, is_bound),

            Seq(ref v) => {
                if v.len() == 0 {
                    fail!("Can't interpret a sequence of zero length");
                } else {
                    self.gen_seq_parser(v.as_slice(), input_ident, is_bound)
                }
            },
            Alt(ref v) => {
                if v.len() == 0 {
                    fail!("Can't interpret a sequence of zero length");
                } else {
                    self.gen_alt_parser(v.as_slice(), input_ident, is_bound)
                }
            },
            Label(id, ref e) => {
                let parser = self.generate_parser_expr(&**e, input_ident, true);
                quote_expr!(self.cx,
                    match $parser {
                        Err(e) => Err(e),
                        Ok((val, s)) => {
                            bindings.$id = Some(val.clone());
                            Ok((val, s))
                        },
                    }
                )
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
                    Ok(($input_ident.slice_to(cr.next),
                        $input_ident.slice_from(cr.next)))
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
                Ok(($input_ident.slice_to(cr.next),
                    $input_ident.slice_from(cr.next)))
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
                    Ok(($input_ident.slice_to($n),
                        $input_ident.slice_from($n)))
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
                    Ok(($input_ident.slice_to(cr.next),
                        $input_ident.slice_from(cr.next)))
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
            super::$n::parse($input_ident)
        )
    }

    fn gen_poslookahead_parser(
        &self,
        exp: &Expression,
        input_ident: libsyn::Ident,
        is_bound: bool,
    ) -> Gc<libsyn::Expr> {
        let parser = self.generate_parser_expr(exp, input_ident, is_bound);
        let val_expr = if is_bound {
            quote_expr!(self.cx, ())
        } else {
            quote_expr!(self.cx, "")
        };

        quote_expr!(self.cx,
            {
                match $parser {
                    Ok(_) => Ok(($val_expr, $input_ident)),
                    Err(e) => Err(e),
                }
            }
        )
    }

    fn gen_neglookahead_parser(
        &self,
        exp: &Expression,
        input_ident: libsyn::Ident,
        is_bound: bool,
    ) -> Gc<libsyn::Expr> {
        let parser = self.generate_parser_expr(exp, input_ident, is_bound);
        let val_expr = if is_bound {
            quote_expr!(self.cx, ())
        } else {
            quote_expr!(self.cx, "")
        };
        quote_expr!(self.cx,
            {
                match $parser {
                    Ok(_) => Err(format!("Could not match ! expression")),
                    Err(_) => Ok(($val_expr, $input_ident)),
                }
            }
        )
    }

    fn gen_star_parser(
        &self,
        exp: &Expression,
        input_ident: libsyn::Ident,
        is_bound: bool,
    ) -> Gc<libsyn::Expr> {
        let inp_ident = libsyn::Ident::new(libsyn::intern("inp"));
        let parser = self.generate_parser_expr(exp, inp_ident, is_bound);
        // the "if true" thing is a hack for type inference
        if is_bound {
            quote_expr!(self.cx,
                {
                    let mut inp = $input_ident;
                    let mut vals = vec!();
                    loop {
                        match $parser {
                            Ok((val, rem)) => {
                                inp = rem;
                                vals.push(val);
                            },
                            Err(_) => break,
                        }
                    }
                    if true {
                        Ok((vals, inp))
                    } else {
                        Err("".to_string())
                    }
                }
            )
        } else {
            quote_expr!(self.cx,
                {
                    let mut inp = $input_ident;
                    loop {
                        match $parser {
                            Ok((_, rem)) => inp = rem,
                            Err(_) => break,
                        }
                    }
                    if true {
                        Ok(($input_ident.slice_to($input_ident.len() - inp.len()), 
                            inp))
                    } else {
                        Err("".to_string())
                    }
                }
            )
        }
    }

    fn gen_plus_parser(
        &self,
        exp: &Expression,
        input_ident: libsyn::Ident,
        is_bound: bool,
    ) -> Gc<libsyn::Expr> {
        let inp_ident = libsyn::Ident::new(libsyn::intern("inp"));
        let parser = self.generate_parser_expr(exp, inp_ident, is_bound);

        if is_bound {
            quote_expr!(self.cx,
                {
                    let mut inp = $input_ident;
                    let mut vals = vec!();
                    match $parser {
                        Err(e) => Err(e),
                        Ok((val, rem)) => {
                            inp = rem;
                            vals.push(val);

                            loop {
                                match $parser {
                                    Ok((val, rem)) => {
                                        inp = rem;
                                        vals.push(val);
                                    },
                                    Err(_) => break,
                                }
                            }

                            Ok((vals, inp))
                        },
                    }
                }
            )
        } else {
            quote_expr!(self.cx,
                {
                    let mut inp = $input_ident;
                    match $parser {
                        Err(e) => Err(e),
                        Ok((_, rem)) => {
                            inp = rem;

                            loop {
                                match $parser {
                                    Ok((_, rem)) => inp = rem,
                                    Err(_) => break,
                                }
                            }

                            Ok(($input_ident.slice_to($input_ident.len() - inp.len()), 
                                inp))
                        },
                    }
                }
            )
        }
    }

    fn gen_opt_parser(
        &self,
        exp: &Expression,
        input_ident: libsyn::Ident,
        is_bound: bool,
    ) -> Gc<libsyn::Expr> {
        let parser = self.generate_parser_expr(exp, input_ident, is_bound);
        if is_bound {
            quote_expr!(self.cx,
                match $parser {
                    Ok((val, rem)) => Ok((Some(val), rem)),
                    Err(_) => Ok((None, $input_ident)),
                }
            )
        } else {
            quote_expr!(self.cx,
                match $parser {
                    Ok((val, rem)) =>
                        Ok(($input_ident.slice_to($input_ident.len() - rem.len()), rem)),

                    Err(_) =>
                        Ok(("", $input_ident)),
                }
            )
        }
    }


    fn gen_seq_parser(
        &self,
        exprs: &[Expression],
        input_ident: libsyn::Ident,
        is_bound: bool,
    ) -> Gc<libsyn::Expr> {
        if exprs.len() == 0 {
            fail!("Don't call gen_seq_parser with a slice of length 0")
        } else if exprs.len() == 1 {
            let parser = self.generate_parser_expr(&exprs[0], input_ident, is_bound);
            quote_expr!(self.cx, $parser)
        } else {
            let parser = self.generate_parser_expr(&exprs[0], input_ident, is_bound);
            let rem = libsyn::Ident::new(libsyn::intern("rem"));
            let parser2 = self.gen_seq_parser(exprs.slice_from(1), rem, is_bound);
            quote_expr!(self.cx,
                match $parser {
                    Err(e) => Err(e),
                    Ok((_, rem)) => $parser2,
                }
            )
        }
    }

    fn gen_alt_parser(
        &self,
        exprs: &[Expression],
        input_ident: libsyn::Ident,
        is_bound: bool,
    ) -> Gc<libsyn::Expr> {
        if exprs.len() == 0 {
            fail!("Don't call gen_alt_parser with a slice of length 0")
        } else if exprs.len() == 1 {
            let parser = self.generate_parser_expr(&exprs[0], input_ident, is_bound);
            quote_expr!(self.cx, $parser)
        } else {
            let parser = self.generate_parser_expr(&exprs[0], input_ident, is_bound);
            let parser2 = self.gen_alt_parser(exprs.slice_from(1), input_ident, is_bound);
            quote_expr!(self.cx,
                match $parser {
                    Err(_) => $parser2,
                    Ok((val, rem)) => Ok((val, rem)),
                }
            )

        }
    }

    // find all of the bindings in an expression.
    // it's dubious that this should be a method of Compiler, but it seems
    // to be the easiest way to quote a type.
    fn find_bindings(&self, expr: &Expression, map: &mut Bindings) {
        match *expr {
            Label(id, _) => {
                // FIXME: check the expression to allow other types
                let ty = quote_ty!(self.cx, &'a str);
                map.insert(id, ty);
            },
            Seq(ref v) => {
                for e in v.iter() {
                    self.find_bindings(e, map);
                }
            },
            Alt(ref v) => {
                for e in v.iter() {
                    self.find_bindings(e, map);
                }
            },
            Optional(ref e) => self.find_bindings(&**e, map),
            ZeroOrMore(ref e) => self.find_bindings(&**e, map),
            OneOrMore(ref e) => self.find_bindings(&**e, map),
            PosLookahead(ref e) => self.find_bindings(&**e, map),
            NegLookahead(ref e) => self.find_bindings(&**e, map),
            _ => {},
        }
    }
}
