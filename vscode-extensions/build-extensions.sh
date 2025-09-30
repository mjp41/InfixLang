#!/bin/bash

# Script to build VSIX packages for Infix and Trieste VS Code extensions
# Requires vsce (Visual Studio Code Extension Manager) to be installed
# Install with: npm install -g vsce

set -e  # Exit on any error

echo "Building VS Code extensions..."

# Check if vsce is installed
if ! command -v vsce &> /dev/null; then
    echo "Error: vsce is not installed. Please install it with:"
    echo "npm install -g vsce"
    exit 1
fi

# Get the script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_DIR="$SCRIPT_DIR/dist"

# Create build directory
mkdir -p "$BUILD_DIR"

echo "Building Infix Language Support extension..."
cd "$SCRIPT_DIR/infix"
vsce package --out "$BUILD_DIR/infix-language-support.vsix"

echo "Building Trieste Syntax Highlighting extension..."
cd "$SCRIPT_DIR/trieste"
vsce package --out "$BUILD_DIR/trieste-syntax.vsix"

echo ""
echo "✅ Extensions built successfully!"
echo "📦 VSIX files created in: $BUILD_DIR"
echo "   - infix-language-support.vsix"
echo "   - trieste-syntax.vsix"
echo ""
echo "To install globally:"
echo "   code --install-extension $BUILD_DIR/infix-language-support.vsix"
echo "   code --install-extension $BUILD_DIR/trieste-syntax.vsix"
echo ""
echo "To install in workspace:"
echo "   Copy VSIX files to .vscode/extensions/ and extract"