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

## Building Extensions

### Prerequisites
Install the Visual Studio Code Extension Manager:
```bash
npm install -g vsce
```

### Build VSIX Packages
```bash
# Build both extensions
./build-extensions.sh

# Or use npm script
npm run build
```

This creates VSIX files in the `dist/` directory:
- `infix-language-support.vsix`
- `trieste-syntax.vsix`

### Installing Extensions

#### Global Installation
```bash
code --install-extension dist/infix-language-support.vsix
code --install-extension dist/trieste-syntax.vsix
```

#### Workspace-Local Installation
The extensions are already installed locally in `.vscode/extensions/` for this workspace.

## Development

### Testing Changes
1. Make changes to extension files
2. Rebuild with `./build-extensions.sh`
3. Reload VS Code window (Ctrl+Shift+P → "Developer: Reload Window")

### Extension Structure
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

## Distribution

The built VSIX files can be:
- Installed locally using `code --install-extension`
- Shared with team members
- Published to the VS Code Marketplace
- Distributed via version control

## Version Control

Both the source extensions and workspace-local copies are tracked in Git:
- Source: `vscode-extensions/`
- Workspace: `.vscode/extensions/`