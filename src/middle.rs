use std::collections::HashMap;

use libsyn;
use front::{mod, Expression, RuleAction};

pub type Grammar = Grammar_<libsyn::Ident>;

struct Grammar_<N> {
    pub name: libsyn::Ident,
    pub rules: HashMap<N, RuleData>,
    pub start: N,
}

struct RuleData {
    pub expr: Expression,
    pub action: Option<RuleAction>,
}


// just convert the Vec of rules into a HashMap of rules, where each rule 
// ident is the key and the expression on the right hand side is the value
//
// If we want the bound value of a non-terminal to be just whatever the
// rule action returns, then for rules without an explicit rule action
// we need some default rule action to fall back on. since bound values
// default to the input matched on that expression, I think the default
// rule action should be the input consumed/matched by the rule, with a
// type of &'a str
//
// TODO: decide whether it makes sense to have consumed input as a variable
// available to every explicit rule action. it would seem we should have this,
// maybe call it _consumed, and then the default rule action is
//
//     &'a str { _consumed }
pub fn convert(g: front::Grammar) -> Option<Grammar> {
    let mut map = HashMap::new();
    let start = g.rules[0].name.clone();
    for front::Rule{name: name, expr: expr, action: action} in g.rules.move_iter() {
        /*
        let ra = match action {
            Some((act) => act
            None => {
                let sess = new_parse_sess();
                let mut p = new_parser("&'a str", &sess);

                let sess2 = new_parse_sess();
                let mut q = new_parser("{ _consumed }", &sess2);

                RuleAction { ty: p.parse_ty(false), expr: q.parse_expr() }
            },
        };

        let data = RuleData { expr: expr, action: ra };
        */
        let data = RuleData { expr: expr, action: action };
        map.insert(name, data);
    }
    Some( Grammar_ { name: g.name, rules: map, start: start })
}

/*
fn new_parser<'a>(s: &str, sess: &'a ParseSess) -> Parser<'a> {
    new_parser_from_source_str(sess, vec!(),
                               "bogus".to_string(),
                               s.to_string())
}
*/
