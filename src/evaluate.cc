#include <infix.h>

// using namespace trieste;

namespace infix {
std::vector<Pass> passes() {
  PassDef operator_defn{
      "structure",
      wf_operator_defn,
      dir::bottomup | dir::once,
      {(Start * T(Operator) * T(Underscore)++[Lhs] * T(Var)[Var] *
                    T(Underscore)++[Rhs] * End) >>
           [](auto &_) {
             return OperatorDef << _[Var] << (Lhs << _[Lhs]) << (Rhs << _[Rhs]);
           },

       T(Group) << ((!T(Operator))++ * (T(Operator)[Operator] << End)) >>
           [](auto &_) { return Error << (ErrorAst << _(Operator)); }}};

  // PassDef operator_grouping {
  //   "operator_grouping",
  //   wf::empty,
  //   dir::topdown | dir::once,
  //   {
  //     T(Group) << (T(Operator) * T(Var)[Var] * T(Underscore)++[Rhs] * End) >>
  //         [](auto &_) {
  //           return Operator << _[Var] << (Rhs << _[Rhs]);
  //         },

  //     T(Group) << (T(Operator) * T(Underscore)++[Lhs] * T(Var)[Var] * End) >>
  //         [](auto &_) {
  //           return Operator << _[Var] << (Lhs << _[Lhs]);
  //         }
  //   }};
  // };

  return {operator_defn};
}
} // namespace infix
  //           T(Class) * T(Var)[Var] * Any++[Body] >>
  //               [](auto &_) { return Class << _[Var] << (Body << _[Body]); },

//           T(Infix) * Any[Lhs] * T(Var)[Var] * (!T(Eq))++[Rhs] * T(Eq) *
//                   Any++[Body] >>
//               [](auto &_) {
//                 return Infix << _[Var] << (Params << _[Lhs] << _[Rhs])
//                              << (Body << _[Body]);
//               },

//           T(Fun) * T(Var)[Var] * (!T(Eq))++[Rhs] * T(Eq) * Any++[Body] >>
//               [](auto &_) {
//                 return Fun << _[Var] << (Params << _[Rhs]) << (Body <<
//                 _[Body]);
//               },

//           In(Params) * T(Var)[Var] >>
//               [](auto &_) { return Param << _[Var] << (Kind << Value); },

//           In(Params) * (T(Paren) << (T(Lazy) * T(Var)[Var])) >>
//               [](auto &_) { return Param << _[Var] << (Kind << Lazy); },

//           In(Params) * (T(Paren) << (T(Type) * T(Var)[Var])) >>
//               [](auto &_) { return Param << _[Var] << (Kind << Type); },

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
//           Any[Lhs] * (T(Group) << Any[Var] * Any++[Rhs]) >>
//               [](auto &_) -> Node {
//             const Node &v = _(Var);
//             auto defs = v->lookup();

//             if (defs.size() == 1 && defs.front()->type() == Infix) {
//               return App << _[Var] << _[Lhs] << _[Rhs];
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
//   //         In(Top) * (T(Class)[Class] << T(Var)[Var]) * T(Semicolon) >>
//   //             [](auto &_) -> Node {
//   //                 logging::Output() << "Process class definition! " <<
//   //                 _(Var)->location().view(); return {}; },
//   //         In(Top) * (T(Fun)[Fun] << ~(T(Infix)[Infix]) * T(Var)[Var]) *
//   //         T(Semicolon) >>
//   //             [](auto &_) -> Node {
//   //                 if (_(Infix))
//   //                     logging::Output() << "Process function definition
//   //                     (Infix): " << _(Var)->location().view();
//   //                 else
//   //                     logging::Output() << "Process function definition: "
//   //                     <<
//   //                     _(Var)->location().view();

//   //                 return {}; },
//   //     }
//   //   };

//   return {structure, application};
// }
// } // namespace infix