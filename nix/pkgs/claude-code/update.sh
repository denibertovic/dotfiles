#!/usr/bin/env nix-shell
#!nix-shell -i bash -p curl jq nix

# Updates claude-code to the latest version from npm.
# Usage: ./update.sh [version]
# If no version is given, fetches the latest from npm.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PACKAGE_NIX="$SCRIPT_DIR/default.nix"

# Get target version
if [[ $# -ge 1 ]]; then
  NEW_VERSION="$1"
else
  NEW_VERSION=$(curl -s https://registry.npmjs.org/@anthropic-ai/claude-code/latest | jq -r '.version')
fi

CURRENT_VERSION=$(grep 'version = ' "$PACKAGE_NIX" | head -1 | sed 's/.*"\(.*\)".*/\1/')

if [[ "$CURRENT_VERSION" == "$NEW_VERSION" ]]; then
  echo "Already at version $NEW_VERSION"
  exit 0
fi

echo "Updating claude-code: $CURRENT_VERSION -> $NEW_VERSION"

# Prefetch the platform tarball and get its hash
echo "Prefetching tarball..."
SRC_HASH=$(nix-prefetch-url --unpack "https://registry.npmjs.org/@anthropic-ai/claude-code-linux-x64/-/claude-code-linux-x64-${NEW_VERSION}.tgz" 2>/dev/null)
SRC_SRI=$(nix hash to-sri --type sha256 "$SRC_HASH" 2>/dev/null || nix hash convert --to sri --type sha256 "$SRC_HASH")

# Update version and hash in default.nix
sed -i "s|version = \"$CURRENT_VERSION\"|version = \"$NEW_VERSION\"|" "$PACKAGE_NIX"
sed -i "s|hash = \"sha256-[^\"]*\"|hash = \"$SRC_SRI\"|" "$PACKAGE_NIX"

echo "Updated to $NEW_VERSION"
echo "  hash: $SRC_SRI"
echo ""
echo "Don't forget to rebuild: home-manager switch --flake .#deni@kanta"
