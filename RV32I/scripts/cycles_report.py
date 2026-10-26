#!/usr/bin/env python3
# Cycle accounting table from tests/*/ipc.txt
import os
import sys

TESTS = sys.argv[1] if len(sys.argv) > 1 else 'RV32I/tests'
COLS = ['cycles', 'instructions', 'ipc', 'commit%', 'head_wait%', 'rob_empty%', 'refill%',
        'mispredicts', 'jalr', 'rob_full', 'freelist', 'lsq_full', 'iq_full', 'rs_full']

print(('%-16s' % 'test') + ''.join('%12s' % c for c in COLS))
for t in sorted(os.listdir(TESTS)):
    f = os.path.join(TESTS, t, 'ipc.txt')
    if not os.path.exists(f):
        continue
    v = dict(line.split() for line in open(f) if line.strip())
    cyc = float(v['cycles']) or 1.0
    v['commit%'] = '%.0f' % (100 * float(v['instructions']) / cyc)
    for k in ('head_wait', 'rob_empty', 'refill'):
        v[k + '%'] = '%.0f' % (100 * float(v[k]) / cyc)
    print(('%-16s' % t) + ''.join('%12s' % v.get(c, '') for c in COLS))
