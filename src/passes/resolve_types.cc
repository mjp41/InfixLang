#include "../infix.h"

#include <deque>
#include <iostream>
#include <map>
#include <unordered_set>
#include <vector>

namespace infix {
enum class ResolutionKind {
  // Not yet processed.
  Uninitialized,
  // Currently being processed, and in the worklist,
  // or cannot be resolved.
  Active,
  // Currently being processed, but not in the worklist.
  // Is in at least one blocked_on set.
  Blocked,
  // Successfully resolved.
  Resolved,
};

struct RelativePath {
  // How many levels up to search; -1 means search all enclosing scopes.
  int lookup_levels{-1};
  // Resolved portion of the path so far (list of path segment nodes).
  Nodes prefix;
  // The final resolved node, if resolution is complete.  This is where the
  // symbol table for the resolved name can be found.  The prefix is required to
  // understand the meaning of generic parameters.  That is this is the module
  // or struct.
  Node node{nullptr};
};

std::ostream &operator<<(std::ostream &os, RelativePath const &rrn) {
  os << "RelativeResolvedName(lookup_levels=" << rrn.lookup_levels
     << ", prefix=[";
  for (auto &p : rrn.prefix) {
    os << p->str();
  }
  os << "])";
  return os;
}

auto ambiguous_lookup_error(Node symtab, Node node) {
  auto results = symtab->look(node->location());
  assert(results.size() > 1);

  Node error_node = Error << (ErrorMsg ^ "Ambiguous lookup:")
                          << (ErrorMsg ^ node->location().str())
                          << (ErrorMsg ^ " found ");
  bool first = true;
  for (auto &res : results) {
    error_node << (ErrorMsg ^ res->location().str());
    if (!first) {
      error_node << (ErrorMsg ^ "and");
    } else {
      first = false;
    }
  }

  return symtab << error_node;
}

std::string type_lookup_to_str(Node type_lookup) {
  std::ostringstream oss;
  bool first = true;
  assert(type_lookup == TypeLookup);
  for (auto &child : *type_lookup) {
    if (!first) {
      oss << "::";
    } else {
      first = false;
    }
    oss << child->str();
  }
  return oss.str();
}

struct ResolutionStatus {
  ResolutionKind kind{ResolutionKind::Uninitialized};
  // Remaining portion that still needs resolution (list of path segment nodes).
  std::deque<Node> pending_suffix;
  // The resolution of the name.
  RelativePath path;
  // Dependents blocked on this entry completing.
  std::unordered_set<Node> dependents;
};
// Worklist of unresolved `use` and `type` bodies gathered during the pass.
std::deque<Node> lookup_worklist;

PassDef get_resolve_types_pass() {
  // Replace names used in types and uses with the fully relative qualified
  // names of their definitions. We only perform this for using definitions:
  //
  // module A[T]
  //   module B
  //     type Foo = T::F
  // module C[U]
  //   use A[U]
  //   type Da = B
  // module D[V]
  //   use C[V]::Da
  //   type F = Foo
  // module E
  //   type G = D[D[Int]]::F
  //
  // In this example we need to resolve generic parameters, but some are also
  // shared.  This allows us to normalise the paths correctly, and share names
  // where possible.  The following illustrates the various steps each name
  // should go through:
  //
  // module A[T]
  //   module B
  //     type Foo = T::F
  // module C[U]
  //   use ..::A[U]
  //   type Da = B
  // module D[V]
  //   use ..::C[V]::Da
  //   use ..::C[V]::B
  //   use ..::C[V]::..::A[..::C[V]::U]::B
  //   use ..::A[..::C[V]::U]::B
  //   use ..::A[V]::B
  //   type F = Foo
  // module E
  //   type G = D[D[Int]]::F
  //
  // Should we update the aliases we resolve on the way?
  // Implementation note:
  //  The pass requires a complex handling of cyclic dependences between name
  //  resolution of using statements and type aliases. Consider the following:
  //
  // module A
  //   use B::Foo
  //   type Bar = Foo
  //
  // module B
  //   use A::Bar
  //   type Foo = Bar
  //
  // In this case there is a cycle between the two modules A and B, and their
  // type aliases Foo and Bar. The resolution must ensure that both type aliases
  // are resolved to the same final type. To handle this we may need to perform
  // multiple passes until a fixed point is reached.
  //
  // module A
  //   use ..::B::Foo
  //   use ..::A::Bar
  //   use ..::B::Foo
  //   use ..::A::Bar
  //   ...
  //   type Bar = ..::B::Foo
  //
  // module B
  //   use ..::A::Bar
  //   ...
  //   type Foo = ..::A::Bar
  //
  // There are cases where this does not lead to a cycle, for instance.

  PassDef pass{"resolve_types",
               wf_function_parse,
               dir::bottomup | dir::once,
               {
                   // Capture the path body for every `use` statement.
                   T(TypeLookup)[TypeLookup] >> [](auto &_) -> Node {
                     lookup_worklist.push_back(_(TypeLookup));
                     return NoChange;
                   },
               }};

  // Debug: dump the gathered bodies after the pass finishes.
  pass.post([](Node) {
    // The core algorithm here requires a worklist of all the things that need
    // resolving. Dependents waiting on a node are tracked in that node's
    // ResolutionStatus.
    NodeMap<ResolutionStatus> resolution_state_;
    std::deque<Node> worklist;

    auto resolution_state = [&](const Node& n) -> ResolutionStatus& {
      assert(n == TypeLookup);
      return resolution_state_[n];
    };

    auto use_to_type_lookup = [](const Node &use_node) -> Node {
      Node type_node = use_node / Type;
      assert(type_node->size() == 1);
      Node type_lookup = type_node->at(0);
      assert(type_lookup == TypeLookup);
      return type_lookup;
    };

    // This brings a node into the algorithm's scope.
    // If it is already known, nothing happens.
    auto add_to_worklist = [&](const Node &n) {
      assert(n == TypeLookup);
      auto &state = resolution_state(n);
      
      assert(state.kind == ResolutionKind::Uninitialized);
      // Ensure an initial state exists; seed pending suffix with a clone of
      // the entry for first-time processing.
      // TODO sanity checking here for different terms
      // TODO generics here.
      for (auto &child : *n) {
        state.pending_suffix.push_back(child);
      }
      worklist.push_back(n);
      resolution_state(n).kind = ResolutionKind::Active;
    };

    auto wait_for = [&](const Node &dependent, const Node &origin) {
      if (resolution_state(origin).kind == ResolutionKind::Resolved) {
        return false;
      }
      // Record that the dependent is waiting on the origin.
      resolution_state(origin).dependents.insert(dependent);
      resolution_state(dependent).kind = ResolutionKind::Blocked;
      return true;
    };

    auto unblock_dependents = [&](const Node &resolved) {
      auto &waiting = resolution_state(resolved).dependents;
      if (waiting.empty()) {
        return;
      }

      for (const auto &dependent : waiting) {
        if (resolution_state(dependent).kind == ResolutionKind::Blocked) {
          worklist.push_back(dependent);
          resolution_state(dependent).kind = ResolutionKind::Active;
        }
      }

      waiting.clear();
    };

    auto lookup_levels_up = [&](const Node &name, const Node &entry) -> RelativePath {
      Node scope = name->scope();
      Nodes using_type_lookups;
      int levels = 0;
      while (scope) {
        auto results = scope->look(name->location());
        if (results.size() > 1) {
          ambiguous_lookup_error(scope, name);
          break;
        }
        if (results.size() == 1) {
          RelativePath result;
          result.lookup_levels = levels;
          result.prefix = {};
          result.node = scope;
          return result;
        }
        // Not found, check the resolved `use` statements in this scope.
        auto current_using = scope->includes();
        for (const auto &u : current_using) {
          // Check if current scope has been resolved
          Node u_node = use_to_type_lookup(u);
          auto &u_state = resolution_state(u_node);
          if (u_state.kind != ResolutionKind::Resolved) {
            // Not resolved add to list to block on.
            assert(u_node == TypeLookup);
            using_type_lookups.push_back(u_node);
            continue;
          }

          auto found = u_state.path.node->look(name->location());
          if (found.size() > 1) {
            ambiguous_lookup_error(u_state.path.node, name);
            continue;
          }

          if (found.size() == 1) {
            // Return found result, and adjust levels according to how far up
            // the using statement was, and add the found name to the prefix.
            RelativePath result = u_state.path;
            result.lookup_levels += levels;
            return result;
          }
        }

        scope = scope->scope();
        levels++;
      }

      // We reach the top without finding it. We need to wait for all the using
      // scopes we found that were unresolved. Note that if there were no
      // unresolved scopes, then the context will stop looking for this, and it
      // will be an error when we complete the resolution.
      for (const auto &u : using_type_lookups) {
        wait_for(entry, u);
      }
      return {};
    };

    auto resolve_use = [&](const Node &entry) {
      auto &state = resolution_state(entry);

      // If already resolved, nothing to do.
      if (state.kind == ResolutionKind::Resolved) {
        return true;
      }

      while (!state.pending_suffix.empty()) {
        Node head = state.pending_suffix.front();

        if (state.path.lookup_levels == -1) {
          state.path = lookup_levels_up(head, entry);
          if (state.path.lookup_levels == -1) {
            // Not found in currently resolved scopes;
            // lookup_level will have added unresolved `use` statements to wait
            // on.
            return false;
          }
        }

        auto found = state.path.node->look(head->location());
        // Should fine either a module/struct or a type alias.
        if (found.size() != 1) {
          ambiguous_lookup_error(state.path.node, head);
          return false;
        }

        if (found.front() == TypeAlias) {
          // Resolve the type alias to continue lookups.
          Node alias_type = found.front() / Type;
          if ((alias_type->size() != 1) && (alias_type->at(0) != TypeLookup)) {
            head << (Error << (ErrorMsg ^ "Invalid type alias body for lookup")
                           << (ErrorAst << head) << (ErrorAst << alias_type));
            return false;
          }

          Node alias_body = alias_type->at(0);
          // If alias not yet resolved, attempt to resolve it; otherwise block
          // until it completes.
          if (wait_for(entry, alias_body))
            return false;

          auto &resolved_alias = resolution_state(alias_body);
          
          // Alias is resolved, add it to the resolved prefix and continue.
          // This involves some form of substitution, and adjustments for
          // relative paths.
          if (state.path.prefix.size() >
              resolved_alias.path.lookup_levels) {
            // Swollow the relative path if there is enough prefix to do so.
            for (size_t i = 0; i < resolved_alias.path.lookup_levels;
                 i++) {
              state.path.prefix.pop_back();
            }
          } else {
            // If there is not enough prefix to swollow, adjust the lookup
            // levels accordingly.
            auto prefix_size = state.path.prefix.size();
            state.path.lookup_levels +=
                resolved_alias.path.lookup_levels - prefix_size;
            state.path.prefix.clear();
          }
          // Add the new prefix segments
          state.path.prefix.insert(
              state.path.prefix.end(),
              resolved_alias.path.prefix.begin(),
              resolved_alias.path.prefix.end());
          // Remove alias name.
          state.pending_suffix.pop_front();
          // Update the resolved_node to the alias's resolved node.
          state.path.node = resolved_alias.path.node;
          continue;
        }

        if (found.front() == Module || found.front() == Struct) {
          // Just a module/struct, extend the resolved prefix and continue.
          state.path.prefix.push_back(head);
          state.pending_suffix.pop_front();
          state.path.node = found.front();
          // TODO: generics need to be handled here?
          continue;
        }

        // Unhandled candidate type.
        // This will result in a failure to resolve, and hence error at the next
        // level.
        // TODO : Here is where would extend to functions and variables if we
        // combine all the resolution into one pass.
        return false;
      }

      return true;
    };

    for (const auto &item : lookup_worklist) {
      add_to_worklist(item);
    }

    while (!worklist.empty()) {
      auto entry = worklist.front();
      worklist.pop_front();
      // During resolution leave as Active status to avoid re-adding cycles.
      assert(resolution_state(entry).kind == ResolutionKind::Active);

      const bool resolved = resolve_use(entry);

      if (resolved) {
        resolution_state(entry).kind = ResolutionKind::Resolved;
        unblock_dependents(entry);
      }
    }

    // Replace all the terms with their resolved names.
    for (const auto &pair : resolution_state_) {
      auto &name = pair.first;
      auto &status = pair.second;
      if (status.kind != ResolutionKind::Resolved) {
        name << (Error << (ErrorMsg ^ "Failed to resolve name:")
                           << (ErrorMsg ^ name->location().str()));
        continue;
      }

      name->erase(name->begin(), name->end());
      for (size_t i = 0; i < status.path.lookup_levels; i++) {
        name << (Parent);
      }
      for (const auto &seg : status.path.prefix) {
        name << seg->clone();
      }
    }

    return static_cast<size_t>(0);
  });

  return pass;
}

} // namespace infix