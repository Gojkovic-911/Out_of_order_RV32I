#!/bin/bash
# Stage one test as out/tb/tests/instr_mem.txt + data_mem.txt
# Usage: prepare.sh <test> <cpu|platform>
#   tb/tests/<suite>/<test>/<test>.s: assembled, golden model check
#   tb/tests/<suite>/<test>/*.c:      built for the target map, host run check
set -e

TEST=$1
TARGET=$2
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
DIR=$(ls -d "$ROOT"/tb/tests/*/"$TEST" | head -1)
SUITE=$(basename "$(dirname "$DIR")")
STAGE="$ROOT/out/tb/tests"

if [ -f "$DIR/$TEST.s" ]; then
    "$ROOT/tb/scripts/compile.sh" "$TEST"
else
    OUT="$STAGE/$SUITE/$TEST/$TARGET"
    PLATFORM_MK=""
    [ "$TARGET" != cpu ] && PLATFORM_MK="$ROOT/fpga/platforms/$TARGET/$TARGET.mk"
    "$ROOT/sw/common/build.sh" "$DIR" "$OUT" "$PLATFORM_MK" --host
    cp "$OUT/instr_mem.txt" "$OUT/data_mem.txt" "$STAGE/"
    cp "$OUT/expected.txt" "$STAGE/$SUITE/$TEST/expected.txt"
fi
