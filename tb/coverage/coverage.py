#!/usr/bin/env python3
# Test coverage of implemented_instructions.md
import glob
import os
import subprocess
import sys
import tempfile

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
TESTS = os.path.join(ROOT, 'tb', 'tests')
OUT = os.path.join(ROOT, 'out', 'tb', 'coverage')

LIST = os.path.join(ROOT, 'tb', 'coverage', 'implemented_instructions.md')
RV32I = [l.split('|')[1].strip() for l in open(LIST)
         if l.startswith('|') and not l.startswith('|---') and 'Instruction' not in l]


def mnemonics(asm):
    with tempfile.TemporaryDirectory() as tmp:
        obj = os.path.join(tmp, 'prog.o')
        subprocess.check_call([os.environ.get('CFG_CROSS', 'riscv64-unknown-elf') + '-as', '-march=rv32i', asm, '-o', obj])
        dis = subprocess.check_output([os.environ.get('CFG_CROSS', 'riscv64-unknown-elf') + '-objdump', '-d', '-M', 'no-aliases', obj], text=True)
    used = set()
    for line in dis.splitlines():
        parts = line.split('\t')
        if len(parts) >= 3 and parts[0].strip().endswith(':'):
            used.add(parts[2].split()[0])
    return used


def pct(used):
    return 100.0 * len(used) / len(RV32I)


rows = []
total = set()
for t in sorted(os.path.basename(d) for d in glob.glob(os.path.join(TESTS, '*', '*'))):
    asm = glob.glob(os.path.join(TESTS, '*', t, t + '.s'))
    asm = asm[0] if asm else ''
    if not os.path.exists(asm):
        continue
    found = mnemonics(asm)
    extra = found - set(RV32I)
    if extra:
        print('%s uses instructions not in the list: %s' % (t, ' '.join(sorted(extra))), file=sys.stderr)
    used = found & set(RV32I)
    total |= used
    rows.append((t, used))

# Per-test reports
os.makedirs(OUT, exist_ok=True)
for t, used in rows:
    with open(os.path.join(OUT, t + '.md'), 'w') as f:
        f.write('# %s: %d/%d instructions (%.0f%%)\n\n| Instruction | Status |\n|---|---|\n'
                % (t, len(used), len(RV32I), pct(used)))
        f.writelines('| %s | %s |\n' % (i, 'covered' if i in used else 'missing') for i in RV32I)

# Summary and instruction x test matrix
lines = ['# Coverage of %s (%d instructions)' % (os.path.relpath(LIST, ROOT), len(RV32I)), '',
         '| Test | Coverage | Missing |', '|---|---|---|']
for t, used in rows:
    lines.append('| %s | %.0f%% | %s |' % (t, pct(used), ' '.join(i for i in RV32I if i not in used) or '-'))
lines.append('| all tests | %.0f%% | %s |' % (pct(total), ' '.join(i for i in RV32I if i not in total) or '-'))
lines += ['', '| Instruction | ' + ' | '.join(t for t, _ in rows) + ' |', '|---' * (len(rows) + 1) + '|']
for i in RV32I:
    lines.append('| %s | ' % i + ' | '.join('x' if i in used else '' for _, used in rows) + ' |')

summary = os.path.join(OUT, 'coverage.md')
open(summary, 'w').write('\n'.join(lines) + '\n')
print('\n'.join(lines[:len(rows) + 5]))
print('Per test: out/tb/coverage/<test>.md, summary: %s' % os.path.relpath(summary, ROOT))
