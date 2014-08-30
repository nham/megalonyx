#![crate_type = "dylib"]
#![feature(plugin_registrar, quote, macro_rules)]

extern crate rustc;
extern crate syntax;

use front::parse_grammar;
use middle::convert;
use back::generate_parser;

use rustc::plugin::Registry;

mod libsyn;
mod front;
mod middle;
mod back;

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
  reg.register_macro("mega", expand)
}

fn expand<'cx>(
    cx: &'cx mut libsyn::ExtCtxt,
    _sp: libsyn::Span, 
    tts: &[libsyn::TokenTree]
) -> Box<libsyn::MacResult+'cx> {

    let mut parser = libsyn::new_parser_from_tts(cx.parse_sess(),
                                                 cx.cfg(),
                                                 Vec::from_slice(tts));
    let grammar = parse_grammar(&mut parser);

    match convert(grammar) {
        None => fail!("Conversion didn't work."),
        Some(g) => {
            let mut rule_parsers = Vec::new();
            for (n, d) in g.rules.iter() {
                let (action_ty, action_expr) = match d.action {
                    Some(a) => (a.ty, a.expr),
                    None => (quote_ty!(&*cx, ()), quote_expr!(&*cx, ())),
                };

                rule_parsers.push( generate_parser(cx, *n, action_ty,
                                                   action_expr, &d.expr));
            }

            let grammar_name = g.name;
            let start_rule = g.start;
            let qi = match g.rules.find(&g.start).unwrap().action {
                Some(a) => {
                    let start_action_ty = a.ty;

                    quote_item!(cx,
                        mod $grammar_name {
                            pub fn parse<'a>(input: &'a str)
                            -> Result<$start_action_ty, String> {
                                match $start_rule(input) {
                                    Err(e) => Err(e),
                                    Ok(s) => Ok(s.val0()),
                                }
                            }

                            $rule_parsers
                        }
                    )
                },
                None => {
                    quote_item!(cx,
                        mod $grammar_name {
                            pub fn parse<'a>(input: &'a str)
                            -> Result<&'a str, String> {
                                match $start_rule(input) {
                                    Err(e) => Err(e),
                                    Ok(s) => Ok(s.val1()),
                                }
                            }

                            $rule_parsers
                        }
                    )
                }
            };

            libsyn::MacItem::new( qi.unwrap() )
        },
    }
}

