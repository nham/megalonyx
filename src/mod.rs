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


enum Value<A> {
    At(A),
    List(Vec<Value<A>>),
}

struct Grammar<A, N> {
    rules: HashMap<N, ParsingExpr<A, N>>,
}

type MatchReturn<'a, A> = (Value<A>, Values<'a, A>);

type Values<'a, A> = &'a [Value<A>];

fn try_match<'a, A, N>(e: ParsingExpr<A, N>, input: Values<'a, A>) 
    -> Result<MatchReturn<'a, A>, ()> {
        Err(())

}

fn main() {
}
