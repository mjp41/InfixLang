#include <trieste/trieste.h>

namespace infix {
using namespace trieste;

inline const auto Instance = TokenDef("instance");
inline const auto Operator =
    TokenDef("operator", flag::lookup | flag::lookdown);
inline const auto Struct = TokenDef("struct");
inline const auto Type = TokenDef("type");
inline const auto Where = TokenDef("where");
inline const auto Eq = TokenDef("eq");
inline const auto Let = TokenDef("let");
inline const auto Or = TokenDef("or");
inline const auto And = TokenDef("and");

inline const auto Indent = TokenDef("indent");

inline const auto Colon = TokenDef("Colon");
inline const auto Comma = TokenDef("Comma");
inline const auto Dot = TokenDef("dot");
inline const auto Paren = TokenDef("paren");
inline const auto Square = TokenDef("square");
inline const auto Hat = TokenDef("hat");
inline const auto Underscore = TokenDef("underscore");
inline const auto Arrow = TokenDef("arrow");
inline const auto Backtick = TokenDef("Backtick");

inline const auto Var = TokenDef("var", flag::print);
inline const auto String = TokenDef("string", flag::print);

inline const auto Class = TokenDef("class");

inline const auto BlankLine = TokenDef("blankline", flag::print);
inline const auto Line = TokenDef("line", flag::print);

inline const auto Lhs = TokenDef("lhs");
inline const auto Rhs = TokenDef("rhs");
inline const auto True = TokenDef("true");
inline const auto False = TokenDef("false");

inline const auto OperatorDef =
    TokenDef("operator_def", flag::lookup | flag::lookdown);

using namespace wf::ops;
inline const auto wf_parse_tokens =
    Group | File | Top | Var | Struct | Paren | Square | Instance | Operator |
    Type | Where | Eq | Let | Or | And | Indent | Colon | Comma | Dot | Paren |
    Square | Underscore | Arrow | Backtick | String | Hat;

inline const auto wf_parser =
    (Top <<= File) | (File <<= Group++) | (Paren <<= wf_parse_tokens++) |
    (Square <<= wf_parse_tokens++) | (Group <<= wf_parse_tokens++) |
    (Indent <<= wf_parse_tokens++);

inline const auto wf_parse_tokens_no_op =
    Group | File | Top | Var | Struct | Paren | Square | Instance | Type |
    Where | Eq | Let | Or | And | Indent | Colon | Comma | Dot | Paren |
    Square | Underscore | Arrow | Backtick | String | Hat;

inline const auto wf_operator_defn =
    (Top <<= File) | (File <<= (Group | OperatorDef)++) | (Paren <<= wf_parse_tokens_no_op++) |
    (Square <<= wf_parse_tokens_no_op++) | (Group <<= wf_parse_tokens_no_op++) |
    (Indent <<= wf_parse_tokens++)
    | (OperatorDef <<= Var * Lhs * Rhs)[Var]
    | (Lhs <<= Underscore++)
    | (Rhs <<= Underscore++);

// inline const auto wf_term = Paren | Var | Group | App;

// inline const auto wf_structure =
//   (Top <<= File)
//   | (File <<= Group++)
//   | (Paren <<= wf_term++)
//   | (Group <<= (Class | Fun | Infix | wf_term)++)
//   | (Fun <<= Var * Params * Body)[Var]
//   | (Infix <<= Var * Params * Body)[Var]
//   | (Class <<= Var * Body)[Var]
//   | (Params <<= Param++)
//   | (Param <<= Var * Kind)[Var]
//   | (Kind <<= Value | Lazy | Type)
//   | (Body <<= wf_term++)
//   | (App <<= wf_term++)
// ;
Parse parser();
std::vector<Pass> passes();
} // namespace infix