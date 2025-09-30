# VS Code Extensions for InfixLang

This directory contains VS Code extensions for syntax highlighting and language support for the InfixLang project.

## Extensions

### 1. Infix Language Support (`infix/`)
- **Language ID**: `infix`
- **File Extensions**: `.infix`, `.ifx`
- **Features**: 
  - Comprehensive syntax highlighting
  - Bracket matching and auto-closing
  - Comment toggling
  - Built-in color theme

### 2. Trieste Syntax Highlighting (`trieste/`)
- **Language ID**: `trieste`
- **File Extensions**: `.trieste`
- **Features**:
  - Syntax highlighting for Trieste AST files
  - Support for length-prefixed strings
  - Built-in color theme

## Workspace Installation

The extensions are automatically available in this workspace via `.vscode/extensions/`. 

No additional setup is required - just open `.infix` or `.trieste` files and enjoy syntax highlighting!

## Extension Structure

Each extension follows VS Code extension conventions:
```
extension-name/
├── package.json           # Extension manifest
├── language-configuration.json  # Language configuration
├── syntaxes/
│   └── *.tmLanguage.json  # TextMate grammar
└── themes/
    └── *-colors.json      # Color theme
```

## Development

### Testing Changes
1. Make changes to extension files in this directory
2. Copy changes to `.vscode/extensions/` if needed
3. Reload VS Code window (Ctrl+Shift+P → "Developer: Reload Window")

### Sharing Extensions
To share with others, they can:
1. Copy the `infix/` and/or `trieste/` directories to their workspace's `.vscode/extensions/`
2. Reload VS Code
3. Extensions will be automatically available for their workspace