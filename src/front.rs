use std::gc::Gc;

use libsyn;

// so lookaheads yield a `()` value.
// also, (label:["abc"])* will result in label being bound
// to the last thing matched inside the star
pub enum Expression_<N> {
    Empty,
    Terminal(char),
    AnyTerminal, // dot operator ('.')
    TerminalString(String), // not in Ford's paper. more compact than a Seq of terminals
    Nonterminal(N),
    Seq(Vec<Expression_<N>>),
    Alt(Vec<Expression_<N>>),
    Optional(Box<Expression_<N>>), // ?
    ZeroOrMore(Box<Expression_<N>>), // *
    OneOrMore(Box<Expression_<N>>), // +
    PosLookahead(Box<Expression_<N>>), // & predicate in Ford's paper
    NegLookahead(Box<Expression_<N>>), // ! predicate in Ford's paper
    Class(String),
    Label(libsyn::Ident, Box<Expression_<N>>), // (label : expr) for binding
}

pub type Expression = Expression_<libsyn::Ident>;

pub struct Grammar {
    pub name: libsyn::Ident,
    pub rules: Vec<Rule>,
}

pub struct Rule {
    pub name: libsyn::Ident,
    pub expr: Expression,
    pub action: Option<RuleAction>,
}

pub struct RuleAction {
    pub ty: Gc<libsyn::Ty>,
    pub expr: Gc<libsyn::Expr>,
}

#[deriving(Show)]
enum ParseError {
    Fail(String),
    NextRule,
    EndOfInput,
}

pub fn parse_grammar(parser: &mut libsyn::Parser) -> Grammar {
    if !consume_grammar_keyword(parser) {
        let tok = parser.this_token_to_string();
        let span = parser.span;
        parser.span_fatal(span,
            format!("Expected grammar declaration of the form `grammar <name> \
                    {{...}}` but found `{}`", tok).as_slice());
    }

    let name = parser.parse_ident();
    parser.expect(&libsyn::LBRACE);
    let mut v = vec!();
    loop {
        match parser.token {
            libsyn::RBRACE => {
                parser.bump();
                break
            },
            _ => v.push( parse_rule(parser) ),
        }
    }
    //thing goes here
    parser.expect(&libsyn::EOF);
    Grammar { name: name, rules: v }
}

fn consume_grammar_keyword(parser: &mut libsyn::Parser) -> bool {
    // the second value attached to IDENT is the "is_mod_name" flag
    match parser.token {
        libsyn::IDENT(ident, false) if "grammar" == ident.as_str() => {
            parser.bump();
            true
        },
        _ => false,
    }
}

fn parse_rule(parser: &mut libsyn::Parser) -> Rule {
    let name = parser.parse_ident();
    parser.expect(&libsyn::EQ);
    match parse_rule_expr(parser) {
        Ok(expr) => {
            // have to check if theres an action for this rule
            let action = match parser.token {
                libsyn::RARROW => {
                    parser.bump();
                    // don't really understand what the parameter of parse_ty does
                    let ty = parser.parse_ty(false);
                    let expr = parser.parse_expr();
                    Some(RuleAction {ty: ty, expr: expr})
                },
                _ => None,
            };

            Rule { name: name,
                   expr: expr,
                   action: action, }
        },
        Err(EndOfInput) => fail!("Unexpected end of input"),
        Err(Fail(s)) => fail!("Failed to parse rule: {}", s),
        _ => fail!("Something bad happened"),
    }
}

// This is how we do it
//
// Definition = Identifier SP '=' SP Expression
// Expression = Sequence (SP '/' SP Sequence)*
// Sequence   = (BoundChunk / Chunk)*
// BoundChunk = Label SP ':' SP Chunk
// Chunk      = PRED? SP Primary SP AMOUNT?
// Primary    = Identifier !(SP '=')
//             / '(' SP Expression SP ')'
//             / Literal
//             / Class
//             / '.'
//
fn parse_rule_expr(parser: &mut libsyn::Parser)
-> Result<Expression, ParseError> {
    let mut choices = vec!();
    loop {
        match parser.token {
            libsyn::RBRACE => break,
            libsyn::EOF => break,
            libsyn::RPAREN => break,
            libsyn::RARROW => break,
            libsyn::IDENT(_, _) => {
                if parser.look_ahead(1, |t| t == &libsyn::EQ) {
                    // This means that we're at the next rule? so stop parsing?
                    break
                } else {
                    // FIXME: this code duplication must stop
                    match parse_rule_choice(parser) {
                        Err(e) => return Err(e),
                        Ok(expr) => choices.push(expr),
                    }
                }
            },
            _ =>
                match parse_rule_choice(parser) {
                    Err(e) => return Err(e),
                    Ok(expr) => choices.push(expr),
                },
        }
    }

    if choices.len() == 0 {
        Err(Fail(format!("Failed to parse an expression")))
    } else if choices.len() == 1 {
        Ok(choices.move_iter().next().unwrap())
    } else {
        Ok(Alt(choices))
    }
}

// parse a sequence of chunks. this forms one "choice", e.g. if we have the
// the expression:
//
//     choice1 / choice2 / ...
//
// then this function will parse choice1 (and discard the slash that follows)
fn parse_rule_choice(parser: &mut libsyn::Parser)
-> Result<Expression, ParseError> {
    let mut chunks = vec!();
    loop {
        match parser.token {
            libsyn::EOF => break,
            libsyn::RBRACE => break,
            libsyn::RPAREN => break,
            libsyn::RARROW => break,
            libsyn::BINOP(libsyn::SLASH) => {
                parser.bump();
                break;
            },
            _ =>
                match parse_rule_chunk(parser) {
                    Ok(expr) => chunks.push(expr),
                    Err(NextRule) => break, // this smells bad to me
                    Err(e) => return Err(e),
                },
        }
    }

    if chunks.len() == 0 {
        Err(Fail(format!("Could not parse any chunks")))
    } else if chunks.len() == 1 {
        Ok(chunks.move_iter().next().unwrap())
    } else {
        Ok(Seq(chunks))
    }
}

fn parse_rule_chunk(parser: &mut libsyn::Parser)
-> Result<Expression, ParseError> {
    match parser.token {
        libsyn::BINOP(libsyn::AND) => {
            parser.bump();
            match parse_rule_chunk(parser) {
                Err(e) => Err(e),
                Ok(expr) => Ok(PosLookahead(box expr)),
            }
        },
        libsyn::NOT => {
            parser.bump();
            match parse_rule_chunk(parser) {
                Err(e) => Err(e),
                Ok(expr) => Ok(NegLookahead(box expr)),
            }
        },
        _ =>
            match parser.token {
                // FIXME: this code duplication must stop
                libsyn::IDENT(id, _) => {
                    if parser.look_ahead(1, |t| t == &libsyn::COLON) {
                        parser.bump();
                        parser.bump();
                        match parse_rule_chunk(parser) {
                            Err(e) => Err(e),
                            Ok(expr) => Ok(Label(id, box expr)),
                        }
                    } else {
                        parse_rule_chunk_no_prefix(parser)
                    }
                },
                _ => parse_rule_chunk_no_prefix(parser),
            },
    }
}


fn parse_rule_chunk_no_prefix(parser: &mut libsyn::Parser)
-> Result<Expression, ParseError> {
    let expr = match parse_primary(parser) {
        Ok(expr) => expr,
        Err(e) => return Err(e),
    };

    match parser.token {
        libsyn::BINOP(libsyn::STAR) => {
            parser.bump();
            Ok( ZeroOrMore(box expr) )
        },
        libsyn::BINOP(libsyn::PLUS) => {
            parser.bump();
            Ok( OneOrMore(box expr) )
        },
        libsyn::QUESTION => {
            parser.bump();
            Ok( Optional(box expr) )
        },
        _ => Ok(expr),
    }
}


// A 'primary' is a char, a string, a dot, a character class, a parenthesized
// expression or a non-terminal
fn parse_primary(parser: &mut libsyn::Parser) -> Result<Expression, ParseError> {
    match parser.token {
        libsyn::LIT_CHAR(name) => {
            parser.bump();
            Ok(Terminal( libsyn::get_name(name).get().char_at(0) ))
        },
        libsyn::LIT_STR(name) => {
            parser.bump();
            Ok(TerminalString( libsyn::get_name(name).get().to_string() ))
        },
        libsyn::DOT => {
            parser.bump();
            Ok(AnyTerminal)
        },
        libsyn::LBRACKET => {
            parser.bump();
            match parser.token {
                libsyn::LIT_STR(name) => {
                    parser.bump();
                    let s = libsyn::get_name(name).get().to_string();

                    match parser.token {
                        libsyn::RBRACKET => {
                            parser.bump();
                            Ok(Class(s))
                        },
                        _ => Err(Fail(format!("Character class must end with ']'"))),
                    }
                },
                _ => Err(Fail(format!("Character class has the form '[\"<chars>\"]'"))),
            }

        },
        libsyn::LPAREN => {
            parser.bump();
            let expr = parse_rule_expr(parser);
            match parser.token {
                libsyn::RPAREN => {
                    parser.bump();
                    expr
                },
                _ => Err(Fail(format!("Mismatched parens"))),
            }
        },
        libsyn::IDENT(id, _) => {
            if parser.look_ahead(1, |t| t == &libsyn::EQ) {
                Err(NextRule)
            } else {
                parser.bump();
                Ok(Nonterminal(id))
            }
        },
        _ => {
            Err(Fail(format!("Couldn't find any non-prefix to parse")))
        },
    }
}

#[cfg(test)]
mod test {
    use syntax::parse::{ParseSess, new_parser_from_source_str, new_parse_sess};
    use syntax::parse::parser::Parser;

    use super::parse_rule_expr;
    use super::{Terminal, AnyTerminal, TerminalString, Class, Optional, Seq, Alt,
                ZeroOrMore, OneOrMore, PosLookahead, NegLookahead, Nonterminal};

    macro_rules! is_variant0(
        ($e:expr, $i:ident) => (match $e { $i => true, _ => false })
    )

    macro_rules! is_variant1(
        ($e:expr, $i:ident) => (match $e { $i(_) => true, _ => false })
    )

    fn new_parser<'a>(s: &str, sess: &'a ParseSess) -> Parser<'a> {
        new_parser_from_source_str(sess, vec!(),
                                   "bogus".to_string(),
                                   s.to_string())
    }

    #[test]
    fn test_parse_char() {
        let sess = new_parse_sess();
        let mut p = new_parser("'c'", &sess);
        assert!( is_variant1!(parse_rule_expr(&mut p).unwrap(), Terminal) );
    }

    #[test]
    fn test_parse_dot() {
        let sess = new_parse_sess();
        let mut p = new_parser(".", &sess);
        assert!( is_variant0!(parse_rule_expr(&mut p).unwrap(), AnyTerminal) );
    }

    #[test]
    fn test_parse_str() {
        let sess = new_parse_sess();
        let mut p = new_parser("\"abc\"", &sess);
        assert!( is_variant1!(parse_rule_expr(&mut p).unwrap(), TerminalString) );
    }

    #[test]
    fn test_parse_nonterminal() {
        let sess = new_parse_sess();
        let mut p = new_parser("abc", &sess);
        let res = parse_rule_expr(&mut p);
        println!("{}", res.is_ok());
        assert!( is_variant1!(res.unwrap(), Nonterminal) );
    }

    #[test]
    fn test_parse_class() {
        let sess = new_parse_sess();
        let mut p = new_parser("[\"abc\"]", &sess);
        assert!( is_variant1!(parse_rule_expr(&mut p).unwrap(), Class) );
    }

    #[test]
    fn test_parse_optional() {
        let sess = new_parse_sess();
        let mut p = new_parser("[\"abc\"]?", &sess);
        assert!( is_variant1!(parse_rule_expr(&mut p).unwrap(), Optional) );
    }

    #[test]
    fn test_parse_zeroormore() {
        let sess = new_parse_sess();
        let mut p = new_parser("[\"abc\"]*", &sess);
        assert!( is_variant1!(parse_rule_expr(&mut p).unwrap(), ZeroOrMore) );
    }

    #[test]
    fn test_parse_oneormore() {
        let sess = new_parse_sess();
        let mut p = new_parser("[\"abc\"]+", &sess);
        assert!( is_variant1!(parse_rule_expr(&mut p).unwrap(), OneOrMore) );
    }

    #[test]
    fn test_parse_poslookahead() {
        let sess = new_parse_sess();
        let mut p = new_parser("&[\"abc\"]+", &sess);
        assert!( is_variant1!(parse_rule_expr(&mut p).unwrap(), PosLookahead) );
    }

    #[test]
    fn test_parse_neglookahead() {
        let sess = new_parse_sess();
        let mut p = new_parser("![\"abc\"]+", &sess);
        assert!( is_variant1!(parse_rule_expr(&mut p).unwrap(), NegLookahead) );
    }

    #[test]
    fn test_parse_seq() {
        let sess = new_parse_sess();
        let mut p = new_parser("![\"abc\"]+ &.", &sess);
        assert!( is_variant1!(parse_rule_expr(&mut p).unwrap(), Seq) );
    }

    #[test]
    fn test_parse_alt() {
        let sess = new_parse_sess();
        let mut p = new_parser("![\"abc\"]+ / 'e'*", &sess);
        assert!( is_variant1!(parse_rule_expr(&mut p).unwrap(), Alt) );
    }

    #[test]
    fn test_parse_parens() {
        let sess = new_parse_sess();
        let mut p = new_parser("!([\"abc\"]+ / ('e' \"abc\")*)", &sess);
        assert!( is_variant1!(parse_rule_expr(&mut p).unwrap(), NegLookahead) );
    }
}
