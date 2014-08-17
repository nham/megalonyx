use std::collections::HashMap;
use std::hash;

type Ident = String;

// p. 29 of Warth's thesis.
// currently missing semantic actions, list patterns
enum ParsingExpr<A, N> {
    Nil,
    Atomic(A),
    Nonterminal(N),
    Seq(Box<ParsingExpr<A, N>>, Box<ParsingExpr<A,N>>),
    Alt(Box<ParsingExpr<A, N>>, Box<ParsingExpr<A,N>>),
    Iter(Box<ParsingExpr<A, N>>),
    NegLookahead(Box<ParsingExpr<A, N>>),
    Binding(Box<ParsingExpr<A, N>>, Ident),
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

// (resulting value from match, remaining (unconsumed) input)
type MatchReturn<'a, A> = (Value<A>, Values<'a, A>);

type Values<'a, A> = &'a [Value<A>];

struct Store<A> {
    bindings: HashMap<Ident, Value<A>>,
}

impl<A> Store<A> {
    fn new() -> Store<A> {
        Store { bindings: HashMap::new() }
    }

    fn set(&mut self, id: Ident, val: Value<A>) {
        self.bindings.insert(id, val);
    }
}

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
    fn try_match<'a>(
        &self,
        e: &ParsingExpr<A, N>,
        input: Values<'a, A>,
        store: &mut Store<A>
    ) -> Result<MatchReturn<'a, A>, ()> {

        match *e {
            Nil => Ok((Zilch, input)),
            Atomic(ref a) => {
                match input {
                    [At(ref b), ..rest] if b == a => Ok((At(b.clone()), rest)),
                    _ => Err(()),
                }
            },
            Nonterminal(ref n) => self.try_match( self.rules.find(n).unwrap(),
                                                  input,
                                                  &mut Store::new() ),
            Seq(ref a, ref b) =>
                match self.try_match(&**a, input, store) {
                    Err(()) => Err(()),
                    Ok((_, vs)) => self.try_match(&**b, vs, store),
                },
            Alt(ref a, ref b) =>
                match self.try_match(&**a, input, store) {
                    Err(()) => self.try_match(&**b, input, store),
                    res@Ok(_) => res,
                },
            Iter(ref a) =>
                match self.try_match(&**a, input, store) {
                    Err(()) => Ok( (List(vec!()), input) ),
                    Ok((List(v), vs)) => {
                        // v is a List(Vec<Value<A>>)
                        // the try_match().val0() is also a List(Vec<Value<A>>)
                        // we want to join the two vecs and wrap it up in a List
                        match self.try_match(e, vs, store) {
                            Ok((List(w), rem)) => Ok( (List(append_all_move(v, w)), rem) ),
                            _ => unreachable!(),
                        }
                    },
                    Ok(_) => unreachable!(),
                },
            NegLookahead(ref a) =>
                match self.try_match(&**a, input, store) {
                    Err(()) => Ok( (Zilch, input) ),
                    Ok(_) => Err(()),
                },
            Binding(ref a, ref id) =>
                match self.try_match(&**a, input, store) {
                    Err(()) => Err(()),
                    Ok((v, vs)) => {
                        // it seems ugly that we're cloning id
                        // maybe Ident should be &str instead?
                        store.set(id.clone(), v.clone());
                        Ok((v, vs))
                    },
                },
        }
    }
}

fn main() {
}
