#include "../infix.h"

namespace infix {

// Forward declaration
Nodes lookup_all(Node n);

PassDef get_resolve_types_pass() {
  // Replace names used in types and uses with the fully relative qualified names of
  // their definitions.
  // We do not use standard absolute paths to correctly handle generic paramters as various levels.
  // For instance, given:
  //
  // module A[T]
  //   module B
  //     type Foo = T::F
  //   module C[U]
  //     use A[U]
  //     type Da = B
  //   module D[V]
  //     use C[V]::Da
  //     type F = Foo
  // module E
  //   type G = D[D[Int]]::F
  // 
  // In this example we need to resolve generic parameters, but some are also shared.  This allows us to normalise
  // the paths correctly, and share names where possible.  The following illustrates the various steps each name
  // should go through:
  //
  // module A[T]
  //   module B
  //     type Foo = ..::T::F
  //   module C[U]
  //     use ..::A[U]
  //     type Da = ..::A[U]::B
  //   module D[V]
  //     use ..::C[V]::..::A[..::C[V]::U]::B
  //     use ..::A[V]::B
  //     type F = ..::A[V]::B::Foo
  //     type F = ..::A[V]::B::..::T::F
  //     type F = ..::A[V]::T::F
  //     type F = V::F
  // module E
  //   type G = ..::D[..::D[..::D::..::std::Int]]::F
  //   type G = ..::D[..::D[..::std::Int]]::F
  //   type G = ..::D[..::D[..::std::Int]]::V::F
  //   type G = ..::D[..::std::Int]::F
  //   type G = ..::D[..::std::Int]::V::F
  //   type G = ..::std::Int::F
  //
  // The end result should be
  //
  // module A[T]
  //   module B
  //     type Foo = ..::T::F
  //   module C[U]
  //     use ..::A[U]
  //     type Da = ..::A[U]::B
  //   module D[V]
  //     use ..::C[V]::..::A[V]::B
  //     use ..::A[V]::B
  //     type F = V::F
  // module E
  //   type G = ..::std::Int::F
  //
  // Implementation note:
  //  The pass requires a complex handling of cyclic dependences between name resolution of
  //  using statements and type aliases.
  //  Consider the following:
  //
  // module A
  //   use B
  //   type Bar = Foo
  //
  // module B
  //   use A
  //   type Foo = Bar
  //
  // In this case there is a cycle between the two modules A and B, and their type aliases Foo and Bar.
  // The resolution must ensure that both type aliases are resolved to the same final type.
  // To handle this we may need to perform multiple passes until a fixed point is reached.
  //
  // module A
  //   use ..::B
  //   type Bar = ..::B::Foo
  //
  // module B
  //   use ..::A
  //   type Foo = ..::A::Bar
  //
  // There are cases where this does not lead to a cycle, for instance:
  //
  // module A
  //   use B
  //   type Bar = Foo
  //
  // module B
  //   use A
  //   type Foo = std::Int
  //
  // In this case the resolution should be:
  //
  // module A
  //   use ..::B
  //   type Bar = ..::B::Foo
  //
  // module B
  //   use ..::A
  //   type Foo = ..::std::Int
  //
  // which is acyclic.

  return PassDef{"resolve_types",
                 wf_function_parse,
                 dir::topdown,
                 {
                     // Empty rule set for now - to be implemented if needed
                 }};
}

} // namespace infix