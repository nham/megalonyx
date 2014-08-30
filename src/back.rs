use libsyn;
use front::{Terminal, AnyTerminal, TerminalString, PosLookahead, NegLookahead,
            Class, ZeroOrMore, OneOrMore, Optional, Seq, Alt, Nonterminal,
            Label, Expression, RuleAction};

use std::gc::Gc;

// Generate a parser for a rule
pub fn generate_parser(
    cx: &mut libsyn::ExtCtxt,
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
    cx: &mut libsyn::ExtCtxt,
    expr: &Expression,
    input_ident: libsyn::Ident,
) -> Gc<libsyn::Expr> {
    match *expr {
        Terminal(c) => {
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
        },
        AnyTerminal => {
            quote_expr!(cx,
                if $input_ident.len() > 0 {
                    let cr = $input_ident.char_range_at(0);
                    Ok($input_ident.slice_from(cr.next))
                } else {
                    Err(format!("Could not match '.' (end of input)"))
                }
            )
        },
        TerminalString(ref s) => {
            let sl = s.as_slice();
            let n = s.len();
            let nbytes = s.as_bytes().len();
            quote_expr!(cx,
                if $input_ident.len() >= $n {
                    if $input_ident.starts_with($sl) {
                        Ok($input_ident.slice_from($nbytes))
                    } else {
                        Err(format!("Could not match '{}': (saw '{}' instead)",
                                    $sl, $input_ident))
                    }
                } else {
                    Err(format!("Could not match '{}' (end of input)", $sl))
                }
            )
        },
        Nonterminal(n) => {
            quote_expr!(cx,
                match $n($input_ident) {
                    Err(e) => Err(e),
                    Ok(s) => Ok(s.val1()),
                }
            )
        },
        PosLookahead(ref e) => {
            let parser = generate_parser_expr(cx, &**e, input_ident);
            quote_expr!(cx,
                {
                    let res: Result<&'a str, String> = $parser;
                    match res {
                        Ok(_) => Ok($input_ident),
                        Err(e) => Err(e),
                    }
                }
            )
        },
        NegLookahead(ref e) => {
            let parser = generate_parser_expr(cx, &**e, input_ident);
            quote_expr!(cx,
                {
                    let res: Result<&'a str, String> = $parser;
                    match res {
                        Ok(_) => Err(format!("Could not match ! expression")),
                        Err(_) => Ok($input_ident),
                    }
                }
            )
        },
        Class(ref s) => {
            let sl = s.as_slice();
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
        },
        ZeroOrMore(ref e) => {
            let parser = generate_parser_expr(cx, &**e, input_ident);
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
        },
        OneOrMore(ref e) => {
            let parser = generate_parser_expr(cx, &**e, input_ident);
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
        },
        Optional(ref e) => {
            let parser = generate_parser_expr(cx, &**e, input_ident);
            quote_expr!(cx,
                match $parser {
                    Ok(rem) => Ok(rem),
                    Err(_) => Ok($input_ident),
                }
            )
        },
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

fn generate_seq_parser(
    cx: &mut libsyn::ExtCtxt,
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
    cx: &mut libsyn::ExtCtxt,
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
