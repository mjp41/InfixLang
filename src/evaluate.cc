#include <infix.h>

// using namespace trieste;

namespace infix {
Nodes lookup_all(Node n) {
  Nodes result;
  Nodes includes = n->lookup();

  for (auto &inc : includes) {
    // If not a use statement, just keep it.
    if (inc != Use) {
      result.push_back(inc);
      continue;
    }

    // TODO: Initially assumed all children are just module names.
    Node start = inc->at(0)->at(0);

    bool first = true;
    for (auto &child : *inc->at(0)) {
      // Check the child is a module name.
      if (child->type() == Name) {
        // TODO Could be from earlier Use statement.
        Nodes child_module;
        if (first) {
          first = false;
          child_module = child->lookup();
          // Filter out Use nodes
          child_module.erase(
              std::remove_if(child_module.begin(), child_module.end(),
                             [](auto &n) { return n->type() == Use; }),
              child_module.end());
        } else {
          child_module = start->lookdown(child->location());
        }
        if (child_module.empty())
          continue;
        if (child_module.size() > 1) {
          // Error: module not found.
          abort();
        }
        if (child_module.front() != Module) {
          // Unhandled case: not a module.
          // TODO: Need to handle type aliases and structs.
          abort();
        }
        // TODO: Handle type parameters here.
        start = child_module.front();
        continue;
      }
      abort();
    }
    auto candidates = start->lookdown(n->location());
    result.insert(result.end(), candidates.begin(), candidates.end());
  }

  return result;
}

std::vector<Pass> passes() {
  auto Decls = T(Struct, TypeAlias, Let, Module, Function, Use);
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
                return Seq << _(Type) << Where
                           << (Body << ExprStack << _[Body]);
              },

          In(Function) * T(Where)[Where] * T(Eq) * Any++[Body] >>
              [](auto &_) {
                return Seq << _(Where) << (Body << ExprStack << _[Body]);
              },

          In(Function) * T(Type)[Type] * (!T(Eq, Body, Where))[Rhs] >>
              [](auto &_) { return _(Type) << _[Rhs]; },

          In(Function) * T(Where)[Where] * (!T(Eq, Body))[Rhs] >>
              [](auto &_) { return _(Where) << _[Rhs]; },

          In(Use) * (Start * T(Name)[Name]/* * ~T(Square)[TypeParam]*/) >>
              [](auto &_) {
                return Path << _(Name)/* << (TypeParam << _(TypeParam))*/;
              },

          In(Use) * T(Path)[Path] * T(DoubleColon) * T(Name)[Name]
                  /* *~T(Square)[TypeParam]*/ >>
              [](auto &_) {
                return _(Path) << _(Name) /*<< (TypeParam << _(TypeParam))*/;
              },

      }};

  // Replace names used in types and uses with the fully qualified names of
  // their definitions.
  PassDef resolve_types{
      "resolve_types",
      wf_function_parse,
      dir::topdown,
      {
      }};


  PassDef infix_parse{
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

  return {operator_defn, function_parse, resolve_types, infix_parse};
}

} // namespace infix