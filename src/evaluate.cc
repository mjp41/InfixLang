#include <infix.h>

namespace infix {

std::vector<Pass> passes() {
  trieste::detail::Pattern MatchType =
      (T(Name) * ~T(Square)) / (T(Paren)) / T(Group);
  auto MatchBody = T(Indent, Paren);

  return {
    get_operator_defn_pass(),
    get_function_parse_pass(),
    get_resolve_types_pass(),
    get_infix_parse_pass()
  };
}

} // namespace infix