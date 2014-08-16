use std::collections::HashMap;

// p. 29 of Warth's thesis.
// currently missing bindings, semantic actions, list patterns
enum ParsingExpr<A, N> {
    Nil,
    Atomic(A),
    Nonterminal(N),
    Seq(Box<ParsingExpr<A, N>>, Box<ParsingExpr<A,N>>),
    Alt(Box<ParsingExpr<A, N>>, Box<ParsingExpr<A,N>>),
    Iter(Box<ParsingExpr<A, N>>),
    NegLookahead(Box<ParsingExpr<A, N>>),
}


#[deriving(PartialEq, Eq, Clone)]
enum Value<A> {
    Zilch,
    At(A),
    List(Vec<Value<A>>),
}

struct Grammar<A, N> {
    rules: HashMap<N, ParsingExpr<A, N>>,
}

type MatchReturn<'a, A> = (Value<A>, Values<'a, A>);

type Values<'a, A> = &'a [Value<A>];

impl<A, N> Grammar<A, N> {
    fn try_match<'a, A, N>(&self, e: ParsingExpr<A, N>, input: Values<'a, A>)
        -> Result<MatchReturn<'a, A>, ()>
        where A: Clone + Eq {

        match e {
            Nil => Ok((Zilch, input)),
            Atomic(a) => {
                match input {
                    [ref b, ..rest] if *b == At(a) => Ok((b.clone(), rest)),
                    _ => Err(()),
                }
            },
            Nonterminal(n) => {
                //match try_match(
                fail!("Unimplemented");

            },
            _ => fail!("Unimplemented"),
        }
    }
}

fn main() {
}
