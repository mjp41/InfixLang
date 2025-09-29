# Infix Language Syntax Highlighting Extension

This VS Code extension provides comprehensive syntax highlighting for Infix language files (`.infix`, `.ifx`).

## Features

- **Comprehensive syntax highlighting** for all Infix language constructs
- **Function declarations** with parameter and type highlighting
- **Struct and type declarations** with proper scoping
- **Operator and keyword highlighting**
- **String and numeric literal support**
- **Comment support** (line and block comments)
- **Auto-closing pairs** for brackets, parentheses, and quotes
- **Smart indentation** rules

## Language Features Supported

### Declarations
- `struct Name[TypeParams]` - Struct definitions
- `function[TypeParams] name (params) : ReturnType = body` - Function definitions  
- `type Name = Type1 | Type2` - Type aliases
- `module Name` - Module declarations
- `let name = value` - Variable declarations

### Type System
- Generic type parameters in `[brackets]`
- Function types with parameter modes (`^` for call-by-name)
- Union types with `|` operator
- Type references (capitalized identifiers)

### Operators
- Arithmetic: `+`, `-`, `*`, `/`, `%`, `**`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Logical: `&&`, `||`, `!`
- Assignment: `=`, `+=`, `-=`, etc.
- Member access: `.`, `::`, `->`

### Comments
- Line comments: `#` and `//`
- Block comments: `/* ... */`

## Example Code

```infix
struct Some[P]
  value: P

function[P] if (condition: True) (thenBranch: ^P) : Option[P] = Some thenBranch
function[P] if (condition: False) (thenBranch: ^P) : Option[P] = None

type Bool = True | False
```

## Installation

This extension is designed to be used as part of the InfixLang project workspace.