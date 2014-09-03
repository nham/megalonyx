use libsyn;
use front::{Terminal, AnyTerminal, TerminalString, PosLookahead, NegLookahead,
            Class, ZeroOrMore, OneOrMore, Optional, Seq, Alt, Nonterminal,
            Label, Expression, RuleAction};
use middle::Grammar;

use std::gc::Gc;

pub fn generate_parsers(
    cx: &libsyn::ExtCtxt,
    grammar: &Grammar,
) -> Vec<Gc<libsyn::Item>> {
    let mut rule_parsers = Vec::new();
    for (n, d) in grammar.rules.iter() {
        let (action_ty, action_expr) = match d.action {
            Some(a) => (a.ty, a.expr),
            None => (quote_ty!(&*cx, ()), quote_expr!(&*cx, ())),
        };

        rule_parsers.push( generate_parser(cx, *n, action_ty,
                                           action_expr, &d.expr));
    }

    rule_parsers
}


// Generate a parser for a rule
pub fn generate_parser(
    cx: &libsyn::ExtCtxt,
    rule_name: libsyn::Ident,
    action_ty: Gc<libsyn::Ty>,
    action_expr: Gc<libsyn::Expr>,
    expr: &Expression,
) -> Gc<libsyn::Item> {
    let input_ident = libsyn::Ident::new(libsyn::intern("input"));
    let parser_contents = generate_parser_expr(cx, expr, input_ident);
    let qi = quote_item!(cx,
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
fn generate_parser_expr(
    cx: &libsyn::ExtCtxt,
    expr: &Expression,
    input_ident: libsyn::Ident,
) -> Gc<libsyn::Expr> {
    match *expr {
        Terminal(c) =>
            generate_terminal_parser(cx, c, input_ident),

        AnyTerminal =>
            generate_anyterminal_parser(cx, input_ident),

        TerminalString(ref s) =>
            generate_terminalstring_parser(cx, s.as_slice(), input_ident),

        Nonterminal(n) =>
            generate_nonterminal_parser(cx, n, input_ident),

        PosLookahead(ref e) =>
            generate_poslookahead_parser(cx, &**e, input_ident),

        NegLookahead(ref e) =>
            generate_neglookahead_parser(cx, &**e, input_ident),

        Class(ref s) =>
            generate_class_parser(cx, s.as_slice(), input_ident),

        ZeroOrMore(ref e) =>
            generate_star_parser(cx, &**e, input_ident),

        OneOrMore(ref e) =>
            generate_plus_parser(cx, &**e, input_ident),

        Optional(ref e) =>
            generate_opt_parser(cx, &**e, input_ident),

        Seq(ref v) => {
            if v.len() == 0 {
                fail!("Can't interpret a sequence of zero length");
            } else {
                generate_seq_parser(cx, v.as_slice(), input_ident)
            }
        },
        Alt(ref v) => {
            if v.len() == 0 {
                fail!("Can't interpret a sequence of zero length");
            } else {
                generate_alt_parser(cx, v.as_slice(), input_ident)
            }
        },
        Label(_, ref e) => {
            // FIXME: currently ignoring bindings
            generate_parser_expr(cx, &**e, input_ident)
        },
        _ => fail!("Unimplemented"),
    }
}


fn generate_terminal_parser(
    cx: &libsyn::ExtCtxt,
    c: char,
    input_ident: libsyn::Ident,
) -> Gc<libsyn::Expr> {
    quote_expr!(cx,
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

fn generate_anyterminal_parser(
    cx: &libsyn::ExtCtxt,
    input_ident: libsyn::Ident,
) -> Gc<libsyn::Expr> {
    quote_expr!(cx,
        if $input_ident.len() > 0 {
            let cr = $input_ident.char_range_at(0);
            Ok($input_ident.slice_from(cr.next))
        } else {
            Err(format!("Could not match '.' (end of input)"))
        }
    )
}


fn generate_terminalstring_parser(
    cx: &libsyn::ExtCtxt,
    sl: &str,
    input_ident: libsyn::Ident,
) -> Gc<libsyn::Expr> {
    let n = sl.len();
    quote_expr!(cx,
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

fn generate_class_parser(
    cx: &libsyn::ExtCtxt,
    sl: &str,
    input_ident: libsyn::Ident,
) -> Gc<libsyn::Expr> {
    quote_expr!(cx,
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

fn generate_nonterminal_parser(
    cx: &libsyn::ExtCtxt,
    n: libsyn::Ident,
    input_ident: libsyn::Ident,
) -> Gc<libsyn::Expr> {
    quote_expr!(cx,
        match $n($input_ident) {
            Err(e) => Err(e),
            Ok(s) => Ok(s.val1()),
        }
    )
}

fn generate_poslookahead_parser(
    cx: &libsyn::ExtCtxt,
    exp: &Expression,
    input_ident: libsyn::Ident,
) -> Gc<libsyn::Expr> {
    let parser = generate_parser_expr(cx, exp, input_ident);
    quote_expr!(cx,
        {
            let res: Result<&'a str, String> = $parser;
            match res {
                Ok(_) => Ok($input_ident),
                Err(e) => Err(e),
            }
        }
    )
}

fn generate_neglookahead_parser(
    cx: &libsyn::ExtCtxt,
    exp: &Expression,
    input_ident: libsyn::Ident,
) -> Gc<libsyn::Expr> {
    let parser = generate_parser_expr(cx, exp, input_ident);
    quote_expr!(cx,
        {
            let res: Result<&'a str, String> = $parser;
            match res {
                Ok(_) => Err(format!("Could not match ! expression")),
                Err(_) => Ok($input_ident),
            }
        }
    )
}

fn generate_star_parser(
    cx: &libsyn::ExtCtxt,
    exp: &Expression,
    input_ident: libsyn::Ident,
) -> Gc<libsyn::Expr> {
    let parser = generate_parser_expr(cx, exp, input_ident);
    let new_fn_name = libsyn::gensym_ident("star");
    quote_expr!(cx,
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

fn generate_plus_parser(
    cx: &libsyn::ExtCtxt,
    exp: &Expression,
    input_ident: libsyn::Ident,
) -> Gc<libsyn::Expr> {
    let parser = generate_parser_expr(cx, exp, input_ident);
    let new_fn_name = libsyn::gensym_ident("plus");
    quote_expr!(cx,
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

fn generate_opt_parser(
    cx: &libsyn::ExtCtxt,
    exp: &Expression,
    input_ident: libsyn::Ident,
) -> Gc<libsyn::Expr> {
    let parser = generate_parser_expr(cx, exp, input_ident);
    quote_expr!(cx,
        match $parser {
            Ok(rem) => Ok(rem),
            Err(_) => Ok($input_ident),
        }
    )
}

fn generate_seq_parser(
    cx: &libsyn::ExtCtxt,
    exprs: &[Expression],
    input_ident: libsyn::Ident,
) -> Gc<libsyn::Expr> {
    if exprs.len() == 0 {
        fail!("Don't call generate_seq_parser with a slice of length 0")
    } else if exprs.len() == 1 {
        let parser = generate_parser_expr(cx, &exprs[0], input_ident);
        quote_expr!(cx, $parser)
    } else {
        let parser = generate_parser_expr(cx, &exprs[0], input_ident);
        let rem = libsyn::Ident::new(libsyn::intern("rem"));
        let parser2 = generate_seq_parser(cx, exprs.slice_from(1), rem);
        quote_expr!(cx,
            match $parser {
                Err(e) => Err(e),
                Ok(rem) => $parser2,
            }
        )
    }
}

fn generate_alt_parser(
    cx: &libsyn::ExtCtxt,
    exprs: &[Expression],
    input_ident: libsyn::Ident,
) -> Gc<libsyn::Expr> {
    if exprs.len() == 0 {
        fail!("Don't call generate_alt_parser with a slice of length 0")
    } else if exprs.len() == 1 {
        let parser = generate_parser_expr(cx, &exprs[0], input_ident);
        quote_expr!(cx, $parser)
    } else {
        let parser = generate_parser_expr(cx, &exprs[0], input_ident);
        let parser2 = generate_alt_parser(cx, exprs.slice_from(1), input_ident);
        quote_expr!(cx,
            match $parser {
                Err(_) => $parser2,
                Ok(rem) => Ok(rem),
            }
        )

    }
}
