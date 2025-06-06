#include <infix.h>

#include <vector>
#include <memory>

using namespace trieste;

namespace infix {
Parse parser() {
  Parse p(depth::file, wf::empty);

  std::shared_ptr<std::vector<size_t>> indents =
      std::make_shared<std::vector<size_t>>();

  std::shared_ptr<size_t> inside_parens =
      std::make_shared<size_t>(0);

  indents->push_back(0);

  auto set_indent = [indents, inside_parens](auto &m, size_t new_indent) {
    if (*inside_parens > 0) {
      if (indents->back() > new_indent)
        m.error("Indentation error: inside parenthses, "
                "indentation must not decrease",
                m.match(1));
      return;
    }

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
      indents->push_back(new_indent);
      m.push(Indent);
    }

    m.term();
  };

  p("start", // this indicates the 'mode' these rules are associated with
    {
        // Ignore empty lines
        "[[:blank:]]*\r?\n" >> [](auto &) {},
        // Ignore comment lines
        "[[:blank:]]*#[^\\n]*\\n" >> [](auto &) {},
        // Match indentation
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
        "#[^\\n]*\\n" >> [](auto &m) { m.mode("start"); }, 

        // ., , and : are special symbols
        "," >> [](auto &m) { m.add(Comma); }, 
        "\\." >> [](auto &m) { m.add(Dot); },
        ":" >> [](auto &m) { m.add(Colon); },
        "=" >> [](auto &m) { m.add(Eq); },
        "`" >> [](auto &m) { m.add(Backtick); },
        "\\^" >> [](auto &m) { m.add(Hat); },
 
        "instance\\b" >> [](auto &m) { m.add(Instance); },
        "operator\\b" >> [](auto &m) { m.add(Operator); },
        "struct\\b" >> [](auto &m) { m.add(Struct); },
        "type\\b" >> [](auto &m) { m.add(Type); },
        "let\\b" >> [](auto &m) { m.add(Let); },
        "where\\b" >> [](auto &m) { m.add(Where); },
        
        // "\"" >> [](auto &m) {
        //   m.add(String);
        // },
        "\"[^\"]*\"" >> [](auto &m) {
          // Match a string literal
          m.add(String); // Add the string as a variable
        },

        // Use ( ) for grouping
        "\\(" >> [inside_parens](auto &m) {
          m.push(Paren);
          (*inside_parens)++;
        },
        "\\)" >>
            [inside_parens](auto &m) {
              m.term({Indent});
              m.pop(Paren);
              (*inside_parens)--;
            },

        // Use [ ] for type params
        "\\[" >> [inside_parens](auto &m) {
          m.push(Square);
          (*inside_parens)++;
        },
        "\\]" >>
            [inside_parens](auto &m) {
              m.term({Indent});
              m.pop(Square);
              (*inside_parens)--;
            },

        // Match complex sequences of symbols/letters and numbers
        "[^[:blank:]\\.\\(\\)\\[\\],:\\^\\n\\r\\\"`]+" >>
            [](auto &m) {
              if (m.match(0).view() == "=")
                m.add(Eq);
              else if (m.match(0).view() == "_")
                m.add(Underscore);
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