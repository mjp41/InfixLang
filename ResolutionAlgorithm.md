# Resolution algorithm

This algorithm is for handling the following types of dependencies:

```
  Ns::= N | N,Ns
  S ::= .. | N | N[T]
  P ::= S::P | S
  T ::= S |  S&S  |  S|S
  e ::= module N es
      | module N[Ns] es
      | type N = P
      | use P
  es ::= e | e es
```

Where `N` is a name (module or type), `T` is a type, `P` is a path expression, and `e` is a module expression.  We use `..` to denote moving up a level in the module hierarchy.

The algorithm is based on Reps-Horwitz-Sagiv (1995) "Precise Interprocedural Dataflow Analysis via Graph Reachability".


We consider each path to be in one of four states:

* unvisited - not yet processed
* unknown start - processed but failed to resolve the start of the path
* reaches type alias - processed path and reaches a type alias, once that alias is complete we can continue on this substituting that path.
* resolved - processed path and fully resolved to the end module.


```
  For each `use` in definition order, resolve the path
        without following type aliases, but with following resolved `use`s.
    If it cannot be resolved, 


```
  Find a type alias, resolve all `use`s in definition order.
  Attempt to resolve using all in scope modules, accumulate `use`s.
  If resolution fails