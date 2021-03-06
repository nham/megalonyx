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
            let c = back::Compiler::new(cx);
            libsyn::MacItem::new( c.compile(&g) )
        },
    }
}

