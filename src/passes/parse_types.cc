#include "../infix.h"

namespace infix {

PassDef get_parse_types_pass() {
  // Normalise type lookups into an explicit chain of `Lookup` nodes, ready for
  // later resolution. This pass rewrites a leading name (with optional type
  // arguments) into a `Lookup`, then repeatedly consumes `::`-separated
  // segments to extend the chain.
  PassDef pass{"parse_types", wf_function_parse, dir::topdown, {
                   // Start of a type: Name [Square]? -> Lookup(Name, Args).
                   In(Type) * Start * T(Name)[Name] * ~T(Square)[Args] >>
                       [](auto &_) {
                         return TypeLookup <= (_(Name) <= *_[Args]);
                       },

                   // Extend lookup chain: Lookup :: Name [Square]? ->
                   // Lookup Lookup.
                   In(Type) * T(TypeLookup)[Lhs] * T(DoubleColon) *
                       T(Name)[Name] * ~T(Square)[Args] >>
                       [](auto &_) {
                         return _(Lhs) <= (_(Name) <= *_[Args]);
                       },
               }};
  return pass;
}

} // namespace infix
