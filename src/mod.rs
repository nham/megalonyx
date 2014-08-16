use std::collections::HashMap;
use std::hash;

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

fn append_all_move<T>(mut a: Vec<T>, b: Vec<T>) -> Vec<T> {
    a.push_all_move(b);
    a
}

// my only question is why Seq's value is just the value of the second application,
// but Iter's value is a list of the value from each application
impl<A, N> Grammar<A, N>
where A: Clone + Eq,
      N: Eq + hash::Hash
{
    fn try_match<'a>(&self, e: &ParsingExpr<A, N>, input: Values<'a, A>)
        -> Result<MatchReturn<'a, A>, ()> {

        match *e {
            Nil => Ok((Zilch, input)),
            Atomic(ref a) => {
                match input {
                    [At(ref b), ..rest] if b == a => Ok((At(b.clone()), rest)),
                    _ => Err(()),
                }
            },
            Nonterminal(ref n) => self.try_match( self.rules.find(n).unwrap(), input ),
            Seq(ref a, ref b) =>
                match self.try_match(&**a, input) {
                    Err(()) => Err(()),
                    Ok((_, vs)) => self.try_match(&**b, vs),
                },
            Alt(ref a, ref b) =>
                match self.try_match(&**a, input) {
                    Err(()) => self.try_match(&**b, input),
                    res@Ok(_) => res,
                },
            Iter(ref a) =>
                match self.try_match(&**a, input) {
                    Err(()) => Ok( (List(vec!()), input) ),
                    Ok((List(v), vs)) => {
                        // v is a List(Vec<Value<A>>)
                        // the try_match().val0() is also a List(Vec<Value<A>>)
                        // we want to join the two vecs and wrap it up in a List
                        match self.try_match(e, vs) {
                            Ok((List(w), rem)) => Ok( (List(append_all_move(v, w)), rem) ),
                            _ => unreachable!(),
                        }
                    },
                    Ok(_) => unreachable!(),
                },
            NegLookahead(ref a) => {
                match self.try_match(&**a, input) {
                    Err(_) => Ok( (Zilch, input) ),
                    Ok(_) => Err(()),
                }
            },
        }
    }
}

fn main() {
}
