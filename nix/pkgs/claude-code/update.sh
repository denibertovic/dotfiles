#!/usr/bin/env nix-shell
#!nix-shell -i bash -p curl jq nix nodejs

# Updates claude-code to the latest version from npm.
# Usage: ./update.sh [version]
# If no version is given, fetches the latest from npm.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PACKAGE_NIX="$SCRIPT_DIR/default.nix"
PACKAGE_LOCK="$SCRIPT_DIR/package-lock.json"

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

# 1. Prefetch the new tarball and get its hash
echo "Prefetching tarball..."
SRC_HASH=$(nix-prefetch-url --unpack "https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-${NEW_VERSION}.tgz" 2>/dev/null)
SRC_SRI=$(nix hash to-sri --type sha256 "$SRC_HASH" 2>/dev/null || nix hash convert --to sri --type sha256 "$SRC_HASH")

# 2. Generate new package-lock.json
echo "Generating package-lock.json..."
TMPDIR=$(mktemp -d)
STORE_PATH=$(nix-store -r "$(nix-prefetch-url --print-path --unpack "https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-${NEW_VERSION}.tgz" 2>/dev/null | tail -1)" 2>/dev/null || true)
# Fallback: use the store path from the prefetch
STORE_PATH="/nix/store/$(ls /nix/store | grep "claude-code-${NEW_VERSION}" | head -1)"
cp "$STORE_PATH/package.json" "$TMPDIR/"
(cd "$TMPDIR" && npm install --package-lock-only --ignore-scripts 2>/dev/null)
cp "$TMPDIR/package-lock.json" "$PACKAGE_LOCK"
rm -rf "$TMPDIR"

# 3. Update version and src hash in default.nix
sed -i "s|version = \"$CURRENT_VERSION\"|version = \"$NEW_VERSION\"|" "$PACKAGE_NIX"
sed -i "s|hash = \"sha256-[^\"]*\"|hash = \"$SRC_SRI\"|" "$PACKAGE_NIX"

# 4. Get new npmDepsHash by doing a failing build
echo "Computing npmDepsHash (this will do a build)..."
sed -i 's|npmDepsHash = "sha256-[^"]*"|npmDepsHash = ""|' "$PACKAGE_NIX"
NPM_HASH=$(NIXPKGS_ALLOW_UNFREE=1 nix-build --expr "with import <nixpkgs> { config.allowUnfree = true; }; callPackage $PACKAGE_NIX {}" 2>&1 \
  | sed -nE 's/.*got: *(sha256-[A-Za-z0-9+/=-]+).*/\1/p')
sed -i "s|npmDepsHash = \"\"|npmDepsHash = \"$NPM_HASH\"|" "$PACKAGE_NIX"

echo "Updated to $NEW_VERSION"
echo "  src hash:      $SRC_SRI"
echo "  npmDepsHash:   $NPM_HASH"
echo ""
echo "Don't forget to rebuild: home-manager switch --flake .#deni@kanta"
