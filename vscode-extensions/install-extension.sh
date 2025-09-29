#!/bin/bash

# Install Infix and Trieste VS Code Extensions locally
# This copies the extensions to VS Code's user extensions directory

TRIESTE_DIR="$HOME/.vscode/extensions/trieste-syntax-0.0.1"
INFIX_DIR="$HOME/.vscode/extensions/infix-language-support-0.0.1"

echo "Installing Infix and Trieste syntax highlighting extensions..."

# Install Trieste extension
echo "Installing Trieste extension..."
mkdir -p "$TRIESTE_DIR"
cp -r vscode-extensions/trieste/* "$TRIESTE_DIR/"
echo "Trieste extension installed to: $TRIESTE_DIR"

# Install Infix extension  
echo "Installing Infix extension..."
mkdir -p "$INFIX_DIR"
cp -r vscode-extensions/infix/* "$INFIX_DIR/"
echo "Infix extension installed to: $INFIX_DIR"

echo ""
echo "Both extensions installed successfully!"
echo "Restart VS Code to activate the extensions."