#include "../infix.h"

namespace infix {

// Forward declaration
Nodes lookup_all(Node n);

PassDef get_resolve_types_pass() {
  // Replace names used in types and uses with the fully qualified names of
  // their definitions.
  return PassDef{
      "resolve_types",
      wf_function_parse,
      dir::topdown,
      {
          // Empty rule set for now - to be implemented if needed
      }};
}

} // namespace infix