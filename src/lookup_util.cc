#include <infix.h>

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

} // namespace infix