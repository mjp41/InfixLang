#include <infix.h>

// using namespace trieste;

namespace infix {
std::vector<Pass> passes() {
  auto Decls = T(Struct, TypeAlias, Let, Module, Function);
  trieste::detail::Pattern MatchType =
      (T(Name) * ~T(Square)) / (T(Paren)) / T(Group);
  auto MatchBody = T(Indent, Paren);

  PassDef operator_defn{
      "structure",
      wf::empty,
      dir::topdown,
      {
          T(Group) << (Decls[Lhs] * Any++[Rhs]) >>
              [](auto &_) { return _(Lhs) << _[Rhs]; },
          T(Paren) << (T(Group)[Group] * End) >>
              [](auto &_) { return Paren << *_(Group); },
          T(Group) << (T(Paren)[Group] * End) >>
              [](auto &_) { return Paren << *_(Group); },

          // Detect offside indentation in the body of a function,
          // struct or module.
          (T(Indent) * T(Indent)[Indent]) * In(Body, Struct, Module) >>
              [](auto &_) {
                return Error << (ErrorMsg ^ "Offside indentation detected!")
                             << (ErrorAst << _[Indent]);
              },

          In(Struct, Module, TypeAlias) * T(Indent)[Indent] * End >>
              [](auto &_) { return Seq << *_(Indent); },

          T(Struct) << (T(Name)[Name] * (~T(Square))[Square] *
                        T(Group, Paren)++[Group] * End) >>
              [](auto &_) {
                return Struct << (_[Name]) << (TypeParams << *_[Square])
                              << (Fields << _[Group]);
              },

          Any[Lhs] * T(Dot) * T(Name)[Name] >>
              [](auto &_) { return Lookup << (Group << _(Lhs)) << _(Name); },

          In(Fields) * T(Group, Paren)
                  << (T(Name)[Name] * T(Colon) * Any++[Type]) >>
              [](auto &_) { return Field << _(Name) << (Type << _[Type]); },

          T(TypeAlias) << (T(Name)[Name] * (~T(Square))[Square] * T(Eq) *
                           Any++[Body] * End) >>
              [](auto &_) {
                return TypeAlias << _[Name] << (TypeParams << *_[Square])
                                 << (Type << _[Body]);
              },

      }};

  PassDef function_parse{
      "function_parse",
      wf_function_parse,
      dir::topdown,
      {

          In(Function) * Start * --T(TypeParams) * (~T(Square))[Square] >>
              [](auto &_) -> Node {
            return Seq << (TypeParams << *_[Square]) << Lhs;
          },

          In(TypeParams) * T(Group)[Group] >>
              [](auto &_) -> Node { return Seq << *_[Group]; },

          In(TypeParams) * T(Name)[Name] >>
              [](auto &_) -> Node { return TypeParam << _(Name); },

          In(Function) * T(Lhs, Rhs)[Lhs] *
                  (T(Group, Paren) << (T(Name)[Name] * T(Colon) * ~T(Hat)[Hat] *
                                       Any++[Type])) >>
              [](auto &_) {
                return _(Lhs)
                       << (Param << _(Name) << (Mode << (_(Hat) ? CBN : CBV))
                                 << (Type << _[Type]));
              },

          In(Function) * T(Lhs, Rhs, Type, Where)[Lhs] *
                  (T(Group, Indent))[Group] >>
              [](auto &_) { return Seq << _(Lhs) << *_(Group); },

          In(Function) * T(Lhs)[Lhs] * T(Name)[Name] * --T(Rhs) >>
              [](auto &_) { return Seq << _(Lhs) << _(Name) << Rhs; },

          In(Function) * T(Rhs)[Rhs] * T(Colon) >>
              [](auto &_) { return Seq << _(Rhs) << Type; },

          In(Function) * T(Type)[Type] * T(Eq) * Any++[Body] >>
              [](auto &_) {
                return Seq << _(Type) << Where << (Body << ExprStack << _[Body]);
              },

          In(Function) * T(Where)[Where] * T(Eq) * Any++[Body] >>
              [](auto &_) { return Seq << _(Where) << (Body << ExprStack << _[Body]); },

          In(Function) * T(Type)[Type] * (!T(Eq, Body, Where))[Rhs] >>
              [](auto &_) { return _(Type) << _[Rhs]; },

          In(Function) * T(Where)[Where] * (!T(Eq, Body))[Rhs] >>
              [](auto &_) { return _(Where) << _[Rhs]; },
      }};

  PassDef infix_parse{
      "infix_parse",
      wf::empty,
      dir::topdown,
      {
          T(Expr) << (Any[Expr] * End) >>
              [](auto &_) { return _(Expr); },
          // Pull decls to top.
          T(ExprStack, PartialCall)[Lhs] * Decls[Rhs] >>
              [](auto &_) { return Seq << _[Rhs] << _[Lhs]; },

          T(ExprStack, PartialCall)[Lhs] * T(Indent, Paren)[Paren] >>
              [](auto &_) {
                return Seq << _(Lhs) << (Expr << ExprStack << *_[Paren]);
              },

          // Ignore groups for now...  This is a line break.  Perhaps use this
          // for disambiguating binary infix and unary prefix for the same
          // symbol i.e.
          // `-` for negative and subtraction

          T(ExprStack)[ExprStack] * End >>
              [](auto &_) { return Seq << *_(ExprStack); },

          T(ExprStack, PartialCall, PartialCreate)[ExprStack] *
                  T(Group)[Group] >>
              [](auto &_) { return Seq << _(ExprStack) << *_(Group); },

          T(ExprStack)[ExprStack] *
                  (!T(Name, PartialCall, PartialCreate))[Rhs] >>
              [](auto &_) { return _(ExprStack) << _[Rhs]; },

          (T(PartialCall)[PartialCall] << (T(Name)[Name] * T(Args)[Args])) *
                  (!T(Name, PartialCall))[Rhs] >>
              [](auto &_) {
                _(Args)->push_back(_[Rhs]);

                auto f = _(Name)->lookup().front();
                auto size = (f / Lhs)->size() + (f / Rhs)->size();
                if (_(Args)->size() == size)
                  return Call << _(Name) << _(Args);
                // Args has been updated in place, so this does actually perform
                // changes.
                return _(PartialCall);
              },

          (T(PartialCreate)[PartialCreate] << (T(Name)[Name] * T(Args)[Args])) *
                  (!T(Name, PartialCreate))[Rhs] >>
              [](auto &_) {
                _(Args)->push_back(_[Rhs]);

                auto f = _(Name)->lookup().front();
                auto size = (f / Fields)->size();
                if (_(Args)->size() == size)
                  return Create << _(Name) << _(Args);
                // Args has been updated in place, so this does actually perform
                // changes.
                return _(PartialCreate);
              },

          T(ExprStack, PartialCall, PartialCreate)[ExprStack] * T(Name)[Name] >>
              [](auto &_) {
                // Is Name a function.

                const Node &v = _(Name);
                auto defs = v->lookup();
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

                if (f->type() != Function && f->type() != Struct) {
                  // Not a function so must be a local variable
                  // TODO Check this is valid.
                  return Seq << _(ExprStack) << (Expr << _(Name));
                }

                auto args_node = NodeDef::create(Args, v->location());

                auto require_lhs_size =
                    f->type() == Function ? (f / Lhs)->size() : 0;
                if (require_lhs_size != 0) {
                  if ((_(ExprStack)->type() == PartialCall) ||
                      (_(ExprStack)->type() == PartialCreate)) {
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

                if (f->type() == Function) {
                  auto require_rhs_size = (f / Rhs)->size();
                  if (require_rhs_size != 0)
                    return Seq << _(ExprStack)
                               << (PartialCall << _(Name) << args_node);

                  return Seq << _(ExprStack) << (Call << _(Name) << args_node);
                }

                auto require_rhs_size = (f / Fields)->size();
                if (require_rhs_size != 0)
                  return Seq << _(ExprStack)
                             << (PartialCreate << _(Name) << args_node);

                return Seq << _(ExprStack) << (Create << _(Name) << args_node);
              },
      }};

  return {operator_defn, function_parse, infix_parse};
}

// PassDef operator_grouping {
//   "operator_grouping",
//   wf::empty,
//   dir::topdown | dir::once,
//   {
//     T(Group) << (T(Operator) * T(Name)[Name] * T(Underscore)++[Rhs] * End)
//     >>
//         [](auto &_) {
//           return Operator << _[Name] << (Rhs << _[Rhs]);
//         },

//     T(Group) << (T(Operator) * T(Underscore)++[Lhs] * T(Name)[Name] * End)
//     >>
//         [](auto &_) {
//           return Operator << _[Name] << (Lhs << _[Lhs]);
//         }
//   }};
// };
} // namespace infix
  //           T(Class) * T(Name)[Name] * Any++[Body] >>
  //               [](auto &_) { return Class << _[Name] << (Body << _[Body]);
  //               },

//           T(Infix) * Any[Lhs] * T(Name)[Name] * (!T(Eq))++[Rhs] * T(Eq) *
//                   Any++[Body] >>
//               [](auto &_) {
//                 return Infix << _[Name] << (Params << _[Lhs] << _[Rhs])
//                              << (Body << _[Body]);
//               },

//           T(Fun) * T(Name)[Name] * (!T(Eq))++[Rhs] * T(Eq) * Any++[Body] >>
//               [](auto &_) {
//                 return Fun << _[Name] << (Params << _[Rhs]) << (Body <<
//                 _[Body]);
//               },

//           In(Params) * T(Name)[Name] >>
//               [](auto &_) { return Param << _[Name] << (Kind << Value); },

//           In(Params) * (T(Paren) << (T(Lazy) * T(Name)[Name])) >>
//               [](auto &_) { return Param << _[Name] << (Kind << Lazy); },

//           In(Params) * (T(Paren) << (T(Type) * T(Name)[Name])) >>
//               [](auto &_) { return Param << _[Name] << (Kind << Type); },

//           T(Group, Body, Paren, Indent)[Top]
//                   << (T(Group, Paren, App)[Group] * End) >>
//               [](auto &_) { return _(Top)->type() << *_[Group]; },

//           T(Indent)[Indent] >> [](auto &_) { return Paren << *_[Indent]; },
//       }};

//   PassDef application{
//       "application",
//       wf_structure,
//       dir::topdown,
//       {
//           Any[Lhs] * (T(Group) << Any[Name] * Any++[Rhs]) >>
//               [](auto &_) -> Node {
//             const Node &v = _(Name);
//             auto defs = v->lookup();

//             if (defs.size() == 1 && defs.front()->type() == Infix) {
//               return App << _[Name] << _[Lhs] << _[Rhs];
//             }
//             return NoChange;
//           },

//           In(Group, Body, Paren) * Start *Any[Lhs] * Any[Rhs] >>
//               [](auto &_) -> Node {
//             const Node &v = _(Rhs);
//             auto defs = v->lookup();

//             if (defs.size() == 1 && defs.front()->type() == Infix) {
//               return App << _[Rhs] << _[Lhs];
//             }

//             return App << _[Lhs] << _[Rhs];
//           },

//           T(App) << (T(App)[Lhs] * Any++[Rhs]) >>
//               [](auto &_) -> Node { return App << *_[Lhs] << _[Rhs]; },
//       }};

//   //   PassDef evaluate{
//   //     "evaluate",
//   //     wf_parser,
//   //     dir::topdown,
//   //     {
//   //         In(Top) * (T(Class)[Class] << T(Name)[Name]) * T(Semicolon) >>
//   //             [](auto &_) -> Node {
//   //                 logging::Output() << "Process class definition! " <<
//   //                 _(Name)->location().view(); return {}; },
//   //         In(Top) * (T(Fun)[Fun] << ~(T(Infix)[Infix]) * T(Name)[Name]) *
//   //         T(Semicolon) >>
//   //             [](auto &_) -> Node {
//   //                 if (_(Infix))
//   //                     logging::Output() << "Process function definition
//   //                     (Infix): " << _(Name)->location().view();
//   //                 else
//   //                     logging::Output() << "Process function definition: "
//   //                     <<
//   //                     _(Name)->location().view();

//   //                 return {}; },
//   //     }
//   //   };

//   return {structure, application};
// }
// } // namespace infix