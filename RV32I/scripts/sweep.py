#!/usr/bin/env python3
# Size sweep: IPC everywhere, synth on a subset
import csv
import itertools
import os
import re
import subprocess
import sys
import time

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
BASE_CFG = os.path.join(ROOT, 'RV32I', 'config.mk')
TESTS_SRC = os.path.join(ROOT, 'RV32I', 'tb', 'tests')
TESTS = os.path.join(ROOT, 'out', 'tests')
SYNTH = os.path.join(ROOT, 'out', 'synth')
OUT = os.path.join(ROOT, 'out', 'sweep')

# Sizes, min to max
SPACE = {
    'ROB_DEPTH': [8, 16, 32, 64],
    'IQ_DEPTH':  [4, 8, 16],
    'LSQ_DEPTH': [4, 8, 16],
}
KEYS = list(SPACE)
SIM_FIELDS = KEYS + ['sim_pass', 'ipc', 'sim_s']
SYNTH_FIELDS = KEYS + ['fmax', 'wns', 'lut', 'lutram', 'ff', 'bram', 'power_w',
                       'dyn_w', 'levels', 'route_pct', 'synth_s', 'path']


def base_values():
    vals = {}
    for line in open(BASE_CFG):
        m = re.match(r'^\s*(\w+)\s*=\s*(.*?)\s*$', line)
        if m:
            vals[m.group(1)] = m.group(2)
    return vals


def name(p):
    return '_'.join('%s%d' % (k.split('_')[0].lower(), p[k]) for k in KEYS)


def sim_points():
    return [dict(zip(KEYS, v)) for v in itertools.product(*SPACE.values())]


def synth_points(default):
    points = [dict(default)]
    for k, values in SPACE.items():
        for v in values:
            points.append(dict(default, **{k: v}))
    n = max(len(v) for v in SPACE.values())
    for i in range(n):
        points.append({k: v[min(i, len(v) - 1)] for k, v in SPACE.items()})
    unique = []
    for p in points:
        if p not in unique:
            unique.append(p)
    return unique


def write_cfg(p, path):
    text = open(BASE_CFG).read()
    for k, v in p.items():
        text = re.sub(r'^(%s\s*=\s*).*$' % k, r'\g<1>%d' % v, text, flags=re.M)
    open(path, 'w').write(text)


def run(cmd, log):
    t0 = time.time()
    with open(log, 'w') as f:
        rc = subprocess.call(cmd, cwd=ROOT, stdout=f, stderr=subprocess.STDOUT)
    return rc, int(time.time() - t0)


def grab(path, pattern):
    try:
        m = re.search(pattern, open(path).read(), re.M)
        return m.group(1) if m else ''
    except OSError:
        return ''


def load(path):
    if not os.path.exists(path):
        return []
    return list(csv.DictReader(open(path)))


def save(path, fields, rows):
    with open(path, 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=fields)
        w.writeheader()
        w.writerows(rows)


def key_of(r):
    return tuple(int(r[k]) for k in KEYS)


def sim_one(p):
    n = name(p)
    cfg = os.path.join(OUT, n + '.mk')
    write_cfg(p, cfg)
    log = os.path.join(OUT, n + '_sim.log')
    _, secs = run(['make', 'sim', 'CONFIG=' + cfg], log)
    ipcs, passed, total = [], 0, 0
    for t in sorted(os.listdir(TESTS_SRC)):
        d = os.path.join(TESTS, t)
        if not (os.path.isdir(os.path.join(TESTS_SRC, t)) and t.endswith('test')):
            continue
        total += 1
        if grab(os.path.join(d, 'comparison_results.txt'), r'VERDICT:\s*(\w+)') == 'PASS':
            passed += 1
            ipc = grab(os.path.join(d, 'ipc.txt'), r'^ipc\s+([\d.]+)')
            if ipc:
                ipcs.append(float(ipc))
    row = dict(p, sim_pass='%d/%d' % (passed, total), sim_s=secs,
               ipc='%.3f' % (sum(ipcs) / len(ipcs)) if passed == total and ipcs else '')
    if passed == total:
        os.remove(log)
    os.remove(cfg)
    return row


def synth_one(p, period):
    n = name(p)
    cfg = os.path.join(OUT, n + '.mk')
    write_cfg(p, cfg)
    log = os.path.join(OUT, n + '_synth.log')
    rc, secs = run(['make', 'synth', 'CONFIG=' + cfg], log)
    util = os.path.join(SYNTH, 'utilization_impl.rpt')
    timing = os.path.join(SYNTH, 'timing_impl.rpt')
    power = os.path.join(SYNTH, 'power.rpt')
    row = dict(p, synth_s=secs)
    if rc == 0:
        wns = grab(timing, r'^\s+WNS\(ns\).*\n.*\n\s+(-?[\d.]+)')
        row.update({
            'wns': wns,
            'fmax': '%.1f' % (1000.0 / (period - float(wns))) if wns else '',
            'lut': grab(util, r'^\| Slice LUTs\s*\|\s*(\d+)'),
            'lutram': grab(util, r'^\|\s+LUT as Memory\s*\|\s*(\d+)'),
            'ff': grab(util, r'^\| Slice Registers\s*\|\s*(\d+)'),
            'bram': grab(util, r'^\| Block RAM Tile\s*\|\s*([\d.]+)'),
            'power_w': grab(power, r'^\| Total On-Chip Power \(W\)\s*\|\s*([\d.]+)'),
            'dyn_w': grab(power, r'^\| Dynamic \(W\)\s*\|\s*([\d.]+)'),
            'route_pct': grab(timing, r'Data Path Delay:.*route [\d.]+ns \(([\d.]+)%\)'),
        })
        try:
            top = open(os.path.join(SYNTH, 'critical_paths.txt')).read().splitlines()[1].split()
            row['levels'], row['path'] = top[1], ' '.join(top[4:])
        except (OSError, IndexError):
            pass
        os.remove(log)
    else:
        row['fmax'] = 'FAIL'
    os.remove(cfg)
    return row


def estimate(p, default, synth):
    """Measured point, else per-dimension model."""
    if key_of(p) in synth:
        return synth[key_of(p)], ''
    base = synth.get(key_of(default))
    if not base or not base.get('lut'):
        return None, ''
    est = {}
    for f in ('lut', 'ff', 'lutram', 'power_w', 'dyn_w'):
        try:
            v = float(base[f])
            for k in KEYS:
                one = synth.get(key_of(dict(default, **{k: p[k]})))
                v += float(one[f]) - float(base[f])
            est[f] = '%.0f' % v if f in ('lut', 'ff', 'lutram') else '%.3f' % v
        except (TypeError, ValueError, KeyError):
            est[f] = ''
    try:
        est['fmax'] = '%.1f' % min(float(synth[key_of(dict(default, **{k: p[k]}))]['fmax'])
                                   for k in KEYS)
    except (TypeError, ValueError, KeyError):
        est['fmax'] = ''
    return est, '~'


def report(default, sims, synth):
    rows = []
    for p in sim_points():
        s = sims.get(key_of(p))
        if not s:
            continue
        hw, mark = estimate(p, default, synth)
        r = dict(p, sim_pass=s['sim_pass'], ipc=s['ipc'], est=mark)
        for f in ('fmax', 'lut', 'ff', 'lutram', 'power_w'):
            r[f] = (hw or {}).get(f, '')
        try:
            r['mips'] = '%.1f' % (float(r['ipc']) * float(r['fmax']))
            r['mips_per_klut'] = '%.2f' % (float(r['mips']) / float(r['lut']) * 1000)
        except (TypeError, ValueError, ZeroDivisionError):
            r['mips'] = r['mips_per_klut'] = ''
        rows.append(r)
    fields = KEYS + ['sim_pass', 'ipc', 'fmax', 'mips', 'lut', 'ff', 'lutram',
                     'power_w', 'mips_per_klut', 'est']
    save(os.path.join(OUT, 'results.csv'), fields, rows)
    rows.sort(key=lambda r: float(r['mips']) if r['mips'] else -1, reverse=True)
    with open(os.path.join(OUT, 'results.md'), 'w') as f:
        f.write('IPC measured everywhere; fmax/area/power measured where est is empty, '
                'estimated per dimension where est is ~. mips = ipc x fmax.\n\n')
        f.write('| ' + ' | '.join(fields) + ' |\n|' + '---|' * len(fields) + '\n')
        for r in rows:
            f.write('| ' + ' | '.join(str(r.get(k, '')) for k in fields) + ' |\n')


def main():
    os.makedirs(OUT, exist_ok=True)
    base = base_values()
    period = 1000.0 / float(base['CPU_CLK_MHZ'])
    default = {k: int(base[k]) for k in KEYS}

    sim_csv = os.path.join(OUT, 'sim.csv')
    synth_csv = os.path.join(OUT, 'synth.csv')
    sims = {key_of(r): r for r in load(sim_csv)}
    synth = {key_of(r): r for r in load(synth_csv)}

    todo = [p for p in sim_points() if key_of(p) not in sims]
    for n, p in enumerate(todo, 1):
        print('[sim %d/%d] %s' % (n, len(todo), name(p)), flush=True)
        sims[key_of(p)] = sim_one(p)
        save(sim_csv, SIM_FIELDS, list(sims.values()))

    todo = [p for p in synth_points(default) if key_of(p) not in synth]
    for n, p in enumerate(todo, 1):
        print('[synth %d/%d] %s' % (n, len(todo), name(p)), flush=True)
        synth[key_of(p)] = synth_one(p, period)
        save(synth_csv, SYNTH_FIELDS, list(synth.values()))

    report(default, sims, synth)
    print('Results: ' + os.path.join(OUT, 'results.md'))


if __name__ == '__main__':
    sys.exit(main())
