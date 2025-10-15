#include <infix.h>

namespace infix {

PassDef get_operator_defn_pass() {
  auto Decls = T(Struct, TypeAlias, Let, Module, Function, Use);
  
  return PassDef{
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
              [](auto &_) { return Lookup << _(Name) << (Args << _(Lhs)); },

          In(Fields) * T(Group, Paren)
                  << (T(Name)[Name] * T(Colon) * Any++[Type]) >>
              [](auto &_) { return Field << _(Name) << (Type << _[Type]); },

          T(TypeAlias) << (T(Name)[Name] * (~T(Square))[Square] * T(Eq) *
                           Any++[Body] * End) >>
              [](auto &_) {
                return TypeAlias << _[Name] << (TypeParams << *_[Square])
                                 << (Type << _[Body]);
              },

          T(Module) << (T(Name)[Name] * (--T(Body) * Any++)[Body] * End) >>
              [](auto &_) { return Module << _(Name) << (Body << _[Body]); },
      }};
}

} // namespace infix