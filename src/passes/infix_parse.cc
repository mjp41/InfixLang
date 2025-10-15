#include "../infix.h"

namespace infix {

// Forward declaration
Nodes lookup_all(Node n);

PassDef get_infix_parse_pass() {
  auto Decls = T(Struct, TypeAlias, Let, Module, Function, Use);
  
  return PassDef{
      "infix_parse",
      wf::empty,
      dir::topdown,
      {
          T(Expr) << T(Expr)[Expr] >> [](auto &_) { return _(Expr); },
          // Pull decls to top.
          T(ExprStack, Partial)[Lhs] * Decls[Rhs] >>
              [](auto &_) { return Seq << _[Rhs] << _[Lhs]; },

          T(ExprStack, Partial)[Lhs] * T(Indent, Paren)[Paren] >>
              [](auto &_) {
                return Seq << _(Lhs) << (Expr << ExprStack << *_[Paren]);
              },

          // Ignore groups for now...  This is a line break.  Perhaps use this
          // for disambiguating binary infix and unary prefix for the same
          // symbol i.e.
          // `-` for negative and subtraction

          T(ExprStack)[ExprStack] * End >>
              [](auto &_) { return Seq << *_(ExprStack); },

          T(ExprStack, Partial)[ExprStack] * T(Group)[Group] >>
              [](auto &_) { return Seq << _(ExprStack) << *_(Group); },

          T(ExprStack)[ExprStack] * (!T(Name, Partial, Eq, SemiColon))[Rhs] >>
              [](auto &_) { return _(ExprStack) << _[Rhs]; },

          (T(Partial)[Partial]
           << (T(Call)[Call] << (T(Name)[Name] * T(Args)[Args]))) *
                  (!T(Name, Partial, Eq, SemiColon))[Rhs] >>
              [](auto &_) {
                _(Args) << _[Rhs];

                auto f = lookup_all(_(Name)).front();
                auto size = (f / Lhs)->size() + (f / Rhs)->size();
                if (_(Args)->size() == size)
                  return Call << _(Name) << _(Args);
                // Args has been updated in place, so this does actually perform
                // changes.
                return _(Partial);
              },

          (T(Partial) << T(Assign)[Assign]) * (!T(Name, Partial))[Rhs] >>
              [](auto &_) { return _(Assign) << _[Rhs]; },

          // Unpack explicit associativity.
          (In(Assign) * ((T(Expr) << T(Assign)[Assign] * End) * End)) >>
              [](auto &_) { return Seq << *_(Assign); },

          (T(Partial) * T(SemiColon)) >>
              [](auto &_) {
                return Error
                       << (ErrorMsg ^ "Unexpected ';' expression not finished.")
                       << (ErrorAst << _[SemiColon]);
              },

          // Semi-colon flushes the evaluation stack.
          T(ExprStack)[ExprStack] * T(SemiColon) >>
              [](auto &_) { return Seq << *_(ExprStack) << ExprStack; },

          (T(Partial)[Partial]
           << (T(Create) << (T(Name)[Name] * T(Args)[Args]))) *
                  (!T(Name, Partial, Eq, SemiColon))[Rhs] >>
              [](auto &_) {
                _(Args)->push_back(_[Rhs]);

                auto f = lookup_all(_(Name)).front();
                auto size = (f / Fields)->size();
                if (_(Args)->size() == size)
                  return Create << _(Name) << _(Args);
                // Args has been updated in place, so this does actually perform
                // changes.
                return _(Partial);
              },

          T(ExprStack)[ExprStack] * T(Eq)[Eq] >>
              [](auto &_) {
                auto es = _(ExprStack)->size();
                if (es < 1) {
                  return Error << (ErrorMsg ^
                                   "No expression for left side of equality")
                               << (ErrorAst << _(Eq));
                }

                auto lhs = _(ExprStack)->back();
                _(ExprStack)->pop_back();
                if (lhs->type() == Assign) {
                  // Chained assignment case. Just need the next expression.
                  return Seq << _(ExprStack) << (Partial << lhs);
                }

                return Seq << _(ExprStack) << (Partial << (Assign << lhs));
              },

          T(ExprStack, Partial)[ExprStack] * T(Name)[Name] >>
              [](auto &_) {
                // Is Name a function.

                const Node &v = _(Name);
                auto defs = lookup_all(v);
                if (defs.size() == 0) {
                  return Error << (ErrorMsg ^ "Name '" + std::string(v->str()) +
                                                  "' is not defined")
                               << (ErrorAst << v);
                }

                const Node &f = defs.front();

                if (defs.size() > 1) {
                  // Check all entries
                  for (const Node &d : defs) {
                    // .. are are functions
                    if (d->type() != Function) {
                      return Error
                             << (ErrorMsg ^ "Shadowing error '" +
                                                std::string(v->type().str()) +
                                                "' is also defined as ...")
                             << (ErrorAst << v);
                    }
                    // Have same lhs and rhs sizes
                    if (((d / Lhs)->size() != (f / Lhs)->size()) ||
                        ((d / Rhs)->size() != (f / Rhs)->size())) {
                      return Error << (ErrorMsg ^ "Mis-matched arity:")
                                   << (ErrorAst << d) << (ErrorAst << f)
                                   << (ErrorMsg ^ "for call at:")
                                   << (ErrorAst << v);
                    }
                  }
                }

                // TODO:  Create for type parameters?
                if (f != Function && f != Struct) {
                  // Not a function so must be a local variable
                  // TODO Check this is valid.
                  return Seq << _(ExprStack) << (Expr << _(Name));
                }

                auto args_node = Args ^ v;

                auto require_lhs_size = f == Function ? (f / Lhs)->size() : 0;
                if (require_lhs_size != 0) {
                  if (_(ExprStack) == Partial) {
                    return Error
                           << (ErrorMsg ^ "Cannot use:") << (ErrorAst << v)
                           << (ErrorMsg ^ "as an argument to ")
                           << (ErrorAst << (_[ExprStack].back() / Name))
                           << (ErrorMsg ^
                               "because it requires " +
                                   std::to_string(require_lhs_size) +
                                   " left arguments. Trying adding parens");
                  }

                  auto es = _(ExprStack)->size();
                  if (es < require_lhs_size)
                    return Error << (ErrorMsg ^
                                     "Not enough left arguments to function '" +
                                         std::string(v->type().str()) + "'")
                                 << (ErrorAst << v);
                  for (size_t i = 0; i < require_lhs_size; i++) {
                    auto last = _(ExprStack)->back();
                    _(ExprStack)->pop_back();
                    args_node->push_front(last);
                  }
                }

                if (f == Function) {
                  auto require_rhs_size = (f / Rhs)->size();
                  if (require_rhs_size != 0)
                    return Seq << _(ExprStack)
                               << (Partial << (Call << _(Name) << args_node));

                  return Seq << _(ExprStack) << (Call << _(Name) << args_node);
                }

                auto require_rhs_size = (f / Fields)->size();
                if (require_rhs_size != 0)
                  return Seq << _(ExprStack)
                             << (Partial << (Create << _(Name) << args_node));

                return Seq << _(ExprStack) << (Create << _(Name) << args_node);
              },
      }};
}

} // namespace infix