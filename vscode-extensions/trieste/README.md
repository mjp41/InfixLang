# Trieste Syntax Highlighting Extension

This VS Code extension provides syntax highlighting for Trieste files (`.trieste`).

## Features

- Syntax highlighting for Trieste AST files
- Support for length-prefixed strings (e.g., `39:Function '(name 4:True)' is not defined`)
- Highlighting of node labels, punctuation, and structure

## Installation

To use this extension in your VS Code workspace:

1. Copy this directory to your VS Code extensions folder, or
2. Use the workspace-specific installation (recommended for development)

## Development

The extension includes:
- `package.json` - Extension manifest
- `language-configuration.json` - Language configuration for brackets, etc.
- `syntaxes/trieste.tmLanguage.json` - TextMate grammar for syntax highlighting

## String Format

Trieste uses length-prefixed strings where `N:` indicates the next N characters are string content:
- `4:True` - String "True" 
- `39:Function '(name 4:True)' is not defined` - String "Function '(name 4:True)' is not defined"