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
pub fn convert(g: front::Grammar) -> Option<Grammar> {
    let mut map = HashMap::new();
    let start = g.rules[0].name.clone();
    for front::Rule{name: name, expr: expr, action: action} in g.rules.move_iter() {
        let data = RuleData { expr: expr, action: action };
        map.insert(name, data);
    }
    Some( Grammar_ { name: g.name, rules: map, start: start })
}
