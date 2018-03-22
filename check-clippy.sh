#!/bin/bash
set -euo pipefail

# Usage: check-clippy.sh [--install]

if cargo install --list | tee /dev/null | grep -q "^clippy v0"; then
    exit 0
fi

if [[ ${1:-""} != "--install" ]]; then
    echo "********************************************************************"
    echo "*  Please install clippy to lint rust code.                        *"
    echo "********************************************************************"
    echo "$0 --install"
    sleep 1
    exit 1
fi

echo "Installing clippy"
cargo +nightly install clippy
