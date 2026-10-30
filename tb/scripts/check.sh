#!/bin/bash
# Check one finished test: golden registers (.s) or a0 vs host run (.c)
# Usage: check.sh <test> <results dir>

TEST=$1
RESULTS=$2
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
DIR=$(ls -d "$ROOT"/tb/tests/*/"$TEST" | head -1)
SUITE=$(basename "$(dirname "$DIR")")

if [ -f "$DIR/$TEST.s" ]; then
    ${CFG_PYTHON:-/usr/bin/python3} "$ROOT/tb/scripts/get_golden_vector.py" "$TEST"
    "$ROOT/tb/scripts/compare_regs.sh" "$TEST" "$RESULTS" | sed 's/\x1b\[[0-9;]*m//g' | grep -E "^VERDICT"
else
    ${CFG_PYTHON:-/usr/bin/python3} "$ROOT/tb/scripts/check_app.py" "$TEST" "$RESULTS" \
        "$ROOT/out/tb/tests/$SUITE/$TEST/expected.txt"
fi
