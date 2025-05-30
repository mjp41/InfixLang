#include <trieste/trieste.h>

namespace infix {
using namespace trieste;

inline const auto Fun = TokenDef("fun", flag::symtab | flag::defbeforeuse | flag::lookup | flag::lookdown);
inline const auto Infix = TokenDef("infix", flag::symtab | flag::defbeforeuse | flag::lookup | flag::lookdown);
inline const auto Lazy = TokenDef("lazy");
inline const auto Lambda = TokenDef("lam");
inline const auto Class = TokenDef("class");
inline const auto Type = TokenDef("type");
inline const auto Let = TokenDef("let");
inline const auto Dot = TokenDef("dot");
inline const auto Eq = TokenDef("eq");
inline const auto Colon = TokenDef("Colon");
inline const auto Paren = TokenDef("paren");
inline const auto Square = TokenDef("square");
inline const auto Var = TokenDef("var", flag::print);

inline const auto Line = TokenDef("line");
inline const auto Indent = TokenDef("indent");
inline const auto Body = TokenDef("body");
inline const auto Params = TokenDef("params");
inline const auto Param = TokenDef("param");
inline const auto Kind = TokenDef("kind");
inline const auto Value = TokenDef("value");
inline const auto App = TokenDef("app");


inline const auto Lhs = TokenDef("lhs");
inline const auto Rhs = TokenDef("rhs");
inline const auto True = TokenDef("true");
inline const auto False = TokenDef("false");


using namespace wf::ops;
inline const auto wf_parse_tokens = Var | Paren | Fun | Infix | Lazy | Class |
                                    Let | Eq | Colon | Square | Type | Type | 
                                    Indent | Dot | Lambda | Group;

inline const auto wf_parser = (Top <<= File) | (File <<= wf_parse_tokens++) |
                              (Paren <<= Group++) |
                              (Indent <<= Group++) |
                              (Square <<= wf_parse_tokens++) |
                              (Group <<= wf_parse_tokens++);

inline const auto wf_term = Paren | Var | Group | App;

inline const auto wf_structure =
  (Top <<= File)
  | (File <<= Group++)
  | (Paren <<= wf_term++)
  | (Group <<= (Class | Fun | Infix | wf_term)++)
  | (Fun <<= Var * Params * Body)[Var]
  | (Infix <<= Var * Params * Body)[Var]
  | (Class <<= Var * Body)[Var]
  | (Params <<= Param++)
  | (Param <<= Var * Kind)[Var]
  | (Kind <<= Value | Lazy | Type)
  | (Body <<= wf_term++)
  | (App <<= wf_term++)
;
Parse parser();
std::vector<Pass> passes();
} // namespace infix