#!/bin/sh

set -e

DIR=$(dirname "$0")

python3 -m py_compile "$DIR/bits571.py"

