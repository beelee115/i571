#!/bin/sh

# TODO: use this script to compile any artifacts needed by your parser,
# assuming that the current directory is the one containing this file.

set -e

DIR=$(dirname "$0")

python3 -m py_compile "$DIR/bits571.py"

