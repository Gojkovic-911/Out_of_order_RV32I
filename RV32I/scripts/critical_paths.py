#!/usr/bin/env python3
# Group timing paths by register family
import collections
import re
import sys


def family(pin):
    p = pin.replace('CPU/data_path_1/', 'dp/').replace('CPU/control_path_1/', 'cp/')
    p = re.sub(r'/[A-Z0-9]+(\[\d+\])?$', '', p)
    p = re.sub(r'_(bret|fret|rep)\w*', '', p)
    p = re.sub(r'\[\d+\]', '[]', p)
    p = re.sub(r'_reg(\[\]|$)', r'\1', p)
    p = re.sub(r'(\[\])+', '[]', p)
    return re.sub(r'\]\[(\w+)\]', r'].\1', p)


groups = collections.OrderedDict()
for line in open(sys.argv[1]):
    slack, levels, delay, start, end = line.strip().split('|')
    key = (family(start), family(end))
    if key not in groups:
        groups[key] = [float(slack), int(levels), float(delay), 0]
    groups[key][3] += 1

limit = int(sys.argv[2]) if len(sys.argv) > 2 else len(groups)
print(f"{'slack':>6} {'levels':>6} {'delay':>6} {'paths':>5}  start -> end")
for (start, end), (slack, levels, delay, n) in list(groups.items())[:limit]:
    print(f"{slack:6.3f} {levels:6d} {delay:6.2f} {n:5d}  {start} -> {end}")
