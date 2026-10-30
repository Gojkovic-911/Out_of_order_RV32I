#!/usr/bin/env python3
# Short status of the last CPU and FPGA runs
import glob
import os
import re
import time

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
OUT = os.path.join(ROOT, 'out')
TESTS = sorted(os.path.basename(d) for d in glob.glob(os.path.join(ROOT, 'tb', 'tests', '*', '*'))
               if os.path.isdir(d))


def when(path):
    return time.strftime('%Y-%m-%d %H:%M', time.localtime(os.path.getmtime(path)))


def tests_line(name, results):
    """PASS count over all tests, failed and missing ones by name."""
    passed, failed, missing, newest = 0, [], [], None
    for t in TESTS:
        f = os.path.join(results, t, 'comparison_results.txt')
        if not os.path.exists(f):
            missing.append(t)
            continue
        newest = max(newest or f, f, key=os.path.getmtime)
        if 'VERDICT: PASS' in open(f).read():
            passed += 1
        else:
            failed.append(t)
    if newest is None:
        return '%-22s not run' % name
    line = '%-22s %d/%d PASS  (%s)' % (name, passed, len(TESTS), when(newest))
    if failed:
        line += '  FAIL: ' + ' '.join(failed)
    if missing:
        line += '  not run: ' + ' '.join(missing)
    return line


def timing(rpt):
    """WNS and WHS from a timing summary report."""
    lines = open(rpt).read().splitlines()
    for i, l in enumerate(lines):
        if l.strip().startswith('WNS(ns)'):
            v = lines[i + 2].split()
            return float(v[0]), float(v[4])
    return None


def luts(rpt):
    m = re.search(r'\|\s*Slice LUTs\*?\s*\|\s*(\d+)', open(rpt).read())
    return m.group(1) if m else '?'


def hw_line(name, timing_rpt, util_rpt, extra=''):
    if not os.path.exists(timing_rpt):
        return '%-22s not run' % name
    t = timing(timing_rpt)
    tim = 'WNS %+.3f ns, WHS %+.3f ns' % t if t else 'timing ?'
    lut = ', %s LUT' % luts(util_rpt) if os.path.exists(util_rpt) else ''
    return '%-22s %s%s%s  (%s)' % (name, tim, lut, extra, when(timing_rpt))


print(tests_line('cpu sim', os.path.join(OUT, 'tb', 'cpu')))
synth = os.path.join(OUT, 'RV32I', 'synth')
print(hw_line('cpu synth', os.path.join(synth, 'timing_impl.rpt'), os.path.join(synth, 'utilization_impl.rpt')))
for d in sorted(glob.glob(os.path.join(OUT, 'tb', 'fpga', '*'))):
    print(tests_line('fpga sim ' + os.path.basename(d), d))
for d in sorted(glob.glob(os.path.join(OUT, 'fpga', '*', 'bitstream'))):
    fw_file = os.path.join(d, 'firmware')
    fw = ', fw ' + open(fw_file).read().strip() if os.path.exists(fw_file) else ''
    updates = [os.path.basename(b)[len('design_1_wrapper_'):-4] for b in glob.glob(os.path.join(d, 'design_1_wrapper_*.bit'))]
    if updates:
        fw += ', fw-update ' + ' '.join(sorted(updates))
    print(hw_line('bitstream ' + os.path.basename(os.path.dirname(d)), os.path.join(d, 'timing.rpt'),
                  os.path.join(d, 'utilization.rpt'), fw))
