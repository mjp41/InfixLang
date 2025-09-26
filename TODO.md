# TODO

* Parsing
  * Lazy
    * Prevent ambiguous overloading with different lazyness
    * 
  * Int and string literals
  * Syntax for pattern matching
    * Just on type?
  * Warn for operator precedence that is not expected...
  * Lambdas? Converting function into a value
    x map (y => y + 1)
  * Uniform syntax over match and lambda
    (x: T => expr)
    and
    match e
      x: T1 => expr1
      x: T2 => expr2
    * Can we do untyped lambda, and match without the var?
      match b
        True => expr1
        False => expr2
      and
        l map (x => x + 1)
    * Perhaps have a 
       group 
         lambda1
         lambda2
         ...
       This forms a group of lambdas that are dispatched by pattern matching.
       

* Type checking

* Code generation
  * Dictionary passing
  * Pattern match
  * Inline function with lazy arguments

* Reference types?   Leave for later as requires more type checking and elaboration to be nice.
  Function may return either a reference type or not.  No disjunction over ref?
     e.g.  (ref Foo) | Int
  This would allow converting .foo into a function symbol, and that could return a reference type.

  Over loading reference types?  Could we provide our own type that has a left and right operator that
  are implicitly handled.  Does this require dynamic dispatch?


# Question

Infix create syntax
```
  struct (left: T) Foo[T] (right: T)
```

Converting function into value?

Optional arguments?
  * `-` is this unary or binary
    `x * - y`
