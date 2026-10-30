#!/usr/bin/env python3
# Application result: a0 (x10) after the run vs the host run
# Usage: check_app.py <app> <results dir> <expected.txt>
import os
import re
import sys

app, results, expected_file = sys.argv[1:4]
dump = open(os.path.join(results, app, 'sim_vector_regs.txt')).read()
phys = int(re.search(r'^x10\s+->\s+reg_([0-9a-fA-F]+)', dump, re.M).group(1), 16)
sim = int(re.search(r'^reg_%d = ([0-9a-fA-F]+)' % phys, dump, re.M).group(1), 16)
exp = int(open(expected_file).read().strip(), 16)

verdict = 'PASS' if sim == exp else 'FAIL'
report = 'VERDICT: %s\na0 expected 0x%08x, simulated 0x%08x\n' % (verdict, exp, sim)
open(os.path.join(results, app, 'comparison_results.txt'), 'w').write(report)
print(report, end='')
