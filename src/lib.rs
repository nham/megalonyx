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
  reg.register_macro("auk", expand)
}

fn expand(
    cx: &mut libsyn::ExtCtxt, 
    _sp: libsyn::Span, 
    tts: &[libsyn::TokenTree]
) -> Box<libsyn::MacResult> {

    let mut parser = libsyn::new_parser_from_tts(cx.parse_sess(),
                                                 cx.cfg(),
                                                 Vec::from_slice(tts));
    let grammar = parse_grammar(&mut parser);

    match convert(grammar) {
        None => fail!("Conversion didn't work."),
        Some(g) => {
            let input = libsyn::Ident::new(libsyn::intern("input"));

            let mut rule_parsers = Vec::new();
            for (n, e) in g.rules.iter() {
                rule_parsers.push( generate_parser(cx, *n, e, input));
            }

            let grammar_name = g.name;
            let start_rule = g.start;
            let qi =
                quote_item!(cx,
                    mod $grammar_name {
                        pub fn parse<'a>(input: &'a str) -> Result<&'a str, String> {
                            $start_rule(input)
                        }

                        $rule_parsers
                    }
                );
            libsyn::MacItem::new( qi.unwrap() )
        },
    }
}

