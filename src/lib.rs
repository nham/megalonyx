#![crate_type = "dylib"]
#![feature(plugin_registrar, quote, macro_rules)]

extern crate rustc;
extern crate syntax;

use front::parse_grammar;
use middle::convert;

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
            let rule_parsers = {
                let c = back::Compiler::new(cx);
                c.generate_parsers(&g)
            };

            let grammar_name = g.name;
            let start_rule = g.start;

            // see below in qi for where 's' comes in to play
            let (return_ty, ok_val) = match g.rules.find(&g.start).unwrap().action {
                Some(a) => ( a.ty, quote_expr!(&*cx, s.val0()) ),
                None => ( quote_ty!(&*cx, &'a str), quote_expr!(&*cx, s.val1()) ),
            };

            let qi =
                quote_item!(cx,
                    mod $grammar_name {
                        type ParseResult<'a, T> = (T, &'a str);

                        pub fn parse<'a>(input: &'a str)
                        -> Result<$return_ty, String> {
                            match $start_rule(input) {
                                Err(e) => Err(e),
                                Ok(s) => Ok($ok_val),
                            }
                        }

                        $rule_parsers
                    }
                );

            libsyn::MacItem::new( qi.unwrap() )
        },
    }
}

