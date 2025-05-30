#include <infix.h>

using namespace trieste;

namespace infix {
Parse parser() {
  Parse p(depth::file, wf_parser);

  std::shared_ptr<std::vector<size_t>> indents =
      std::make_shared<std::vector<size_t>>();

  indents->push_back(0);

  auto set_indent = [indents](auto &m, size_t new_indent) {
    if (indents->back() < new_indent) {
      indents->push_back(new_indent);
      m.push(Indent);
      return;
    }

    while (indents->back() > new_indent) {
      indents->pop_back();
      m.term();
      m.pop(Indent);
    }

    if (indents->back() != new_indent) {
      m.error("Indentation error");
    }

    m.term();
  };

  p("start", // this indicates the 'mode' these rules are associated with
    {
        "[[:blank:]]*\r?\n" >> [](auto &) {},
        "[[:blank:]]*" >>
            [set_indent](auto &m) {
              m.mode("main");
              set_indent(m, m.match(1).len);
            },
    });

  p("main", // this indicates the 'mode' these rules are associated with
    {
        // Ignore white space
        "[[:blank:]]+" >> [](auto &) {},

        // Line based comment
        "//[^\\n]*\\n" >> [](auto &m) { m.mode("start"); }, 

        // . and ; are special characters
        "\\." >> [](auto &m) { m.add(Dot); },
        ";" >> [](auto &m) { m.term(); },
        ":" >> [](auto &m) { m.add(Colon); },

        "class\\b" >> [](auto &m) { m.add(Class); },
        "infix\\b" >> [](auto &m) { m.add(Infix); },
        "fun\\b" >> [](auto &m) { m.add(Fun); },
        "let\\b" >> [](auto &m) { m.add(Let); },
        "lam\\b" >> [](auto &m) { m.add(Lambda); },
        "lazy\\b" >> [](auto &m) { m.add(Lazy); },
        "type\\b" >> [](auto &m) { m.add(Type); },

        // Use ( ) for grouping
        "\\(" >> [](auto &m) { m.push(Paren); },
        "\\)" >>
            [](auto &m) {
              m.term();
              m.pop(Paren);
            },

        // Use [ ] for type params
        "\\[" >> [](auto &m) { m.push(Square); },
        "\\]" >>
            [](auto &m) {
              m.term();
              m.pop(Square);
            },

        // Match complex sequences of symbols/letters and numbers
        "[^[:space:]\\.\\(\\)\\[\\];:]+" >>
            [](auto &m) {
              if (m.match(0).view() == "=")
                m.add(Eq);
              else
                m.add(Var);
            },

        // On new line go back to finding indent
        "\\n" >> [](auto &m) { m.mode("start"); },
    });

  p.done([set_indent](auto &m) { set_indent(m, 0); });

  return p;
}
} // namespace infix