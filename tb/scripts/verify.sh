#!/bin/bash
# Verification before a commit: every make command and configuration combination, one Vivado job at a time
# Usage: verify.sh   (exit 0 only when every run passes; table in out/verify/summary.md, seal for the git hooks)

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
OUT="$ROOT/out/verify"
SUMMARY="$OUT/summary.md"
cd "$ROOT" || exit 1
. "$ROOT/tools/git-hooks/verified.sh"
mkdir -p "$OUT"
rm -f "$OUT"/*.log "$SUMMARY"
KEY=$(tree_key "$(work_tree)")
FAILED=0
START=$(date +%s)

mk_value() {
    sed -n -E "s/^\s*$1\s*=\s*(\S*)\s*$/\1/p" "$2" | head -1
}

# Worst setup slack from a timing summary
wns() {
    grep -A6 "Design Timing Summary" "$1" 2>/dev/null | tail -1 | awk '{print $1}'
}

# Regression rows of verdict.sh: PASS count, failure when any FAIL/MISSING or none passed
tests_result() {
    local rows pass bad
    rows=$(sed 's/\x1b\[[0-9;]*m//g' "$1" | grep -E '^[A-Za-z0-9_]+ +(PASS|FAIL|MISSING)\b')
    pass=$(grep -c ' PASS' <<< "$rows")
    bad=$(grep -cE ' (FAIL|MISSING)' <<< "$rows")
    echo "$pass/$((pass + bad))"
    [ "$bad" -eq 0 ] && [ "$pass" -gt 0 ]
}

# One run: name, check (none|tests|wns:<report>), make arguments
run() {
    local name=$1 check=$2 log="$OUT/$1.log" t rc result ok
    shift 2
    printf '%-40s ' "$name"
    t=$(date +%s)
    make "$@" > "$log" 2>&1
    rc=$?
    t=$(( $(date +%s) - t ))
    ok=$([ $rc -eq 0 ] && echo 1 || echo 0)
    result="rc $rc"
    case $check in
        tests) result=$(tests_result "$log") || ok=0 ;;
        wns:*) result="WNS $(wns "${check#wns:}") ns"
               awk -v w="$(wns "${check#wns:}")" 'BEGIN { exit !(w != "" && w >= 0) }' || ok=0 ;;
    esac
    [ $rc -ne 0 ] && ok=0
    [ $ok -eq 1 ] && verdict=PASS || { verdict=FAIL; FAILED=$((FAILED + 1)); }
    printf '%-5s %-22s %5ss\n' "$verdict" "$result" "$t"
    echo "| \`make $*\` | $verdict | $result | ${t} s |" >> "$SUMMARY"
}

{
    echo "# Verification $(date '+%Y-%m-%d %H:%M'), $(git rev-parse --short HEAD)$(git diff --quiet || echo ' + local changes')"
    echo ""
    echo "| Command | Verdict | Result | Time |"
    echo "|---|---|---|---|"
} > "$SUMMARY"

run check none check

# CPU: read latency and DMEM wait combinations
for lat in 1 2 3; do
    for wait in 0 3; do
        run "cpu_sim_lat${lat}_wait0-${wait}" tests cpu sim DMEM_RD_LATENCY=$lat DATA_MEM_MIN_WAIT=0 DATA_MEM_MAX_WAIT=$wait
    done
done
# Fresh project, the cached one keeps its old file list
rm -rf "$ROOT/out/tb/cpu/RISCV_project"
run cpu_sim_prj tests cpu sim PRJ=1
run cpu_synth "wns:$ROOT/out/RV32I/synth/timing_impl.rpt" cpu synth

# Every platform: regression, firmware run, bitstream, firmware update
board=$(sed -n -E 's/^BOARD\s*\?=\s*(\S+).*/\1/p' Makefile)
for dir in fpga/platforms/*/; do
    p=$(basename "$dir")
    fw=$(mk_value DEFAULT_FW "$dir/$p.mk")
    run "fpga_sim_$p" tests fpga sim PLATFORM=$p
    run "fpga_sim_${p}_fw" none fpga sim PLATFORM=$p FW=$fw
    run "fpga_bitstream_$p" "wns:$ROOT/out/fpga/${p}_$board/bitstream/timing.rpt" fpga bitstream PLATFORM=$p
    run "fpga_fw_update_$p" none fpga fw-update PLATFORM=$p FW=$fw
done

T=$(( $(date +%s) - START ))
echo "" >> "$SUMMARY"
echo "$FAILED failed, $((T / 60)) min $((T % 60)) s" | tee -a "$SUMMARY"
echo "Summary: $SUMMARY"
[ $FAILED -eq 0 ] || exit 1
# Seal: this state may be committed and pushed
[ "$(tree_key "$(work_tree)")" = "$KEY" ] || { echo "Files changed during verification, no seal"; exit 1; }
sealed "$KEY" || echo "$KEY" >> "$SEAL"
echo "Sealed $KEY"
