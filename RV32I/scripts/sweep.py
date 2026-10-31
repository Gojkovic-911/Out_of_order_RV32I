#!/usr/bin/env python3
# Sweep: sizes and RAM styles (sim + synth); --directives: place x route on cpu.mk
import csv
import itertools
import os
import re
import subprocess
import sys
import time

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
BASE_CFG = os.path.join(ROOT, 'RV32I', 'cpu.mk')
TOOLS_MK = os.path.join(ROOT, 'tools', 'tools.mk')
TESTS_SRC = os.path.join(ROOT, 'tb', 'tests', 'stress_tests')
TESTS = os.path.join(ROOT, 'out', 'tb', 'cpu')
PROGS = os.path.join(ROOT, 'out', 'tb', 'tests')
SYNTH = os.path.join(ROOT, 'out', 'RV32I', 'synth')
OUT = os.path.join(ROOT, 'out', 'RV32I', 'sweep')
PLATFORM = os.environ.get('PLATFORM', 'simple')
BOARD = os.environ.get('BOARD', 'zybo')
PLATFORM_MK = os.path.join(ROOT, 'fpga', 'platforms', PLATFORM, PLATFORM + '.mk')
FPGA_OUT = os.path.join(ROOT, 'out', 'fpga', '%s_%s' % (PLATFORM, BOARD))

# Sizes, min to max (IPC and area)
SPACE = {
    'ROB_DEPTH': [8, 16, 32, 64],
    'IQ_DEPTH':  [4, 8, 16],
    'LSQ_DEPTH': [4, 8, 16],
}
# Synthesis only, no IPC effect
RAM_SPACE = {
    'RF_RAM_STYLE': ['block', 'distributed'],
    'IQ_RAM_STYLE': ['block', 'distributed'],
}
# Place x route on the cpu.mk configuration (--directives)
DIRECTIVES = ['Default', 'Explore', 'EarlyBlockPlacement', 'WLDrivenBlockPlacement',
              'ExtraNetDelay_high', 'ExtraNetDelay_low', 'AltSpreadLogic_high',
              'AltSpreadLogic_medium', 'AltSpreadLogic_low', 'ExtraPostPlacementOpt',
              'ExtraTimingOpt']
ROUTES = ['AggressiveExplore', 'NoTimingRelaxation', 'MoreGlobalIterations']
# WNS below this margin is fragile (ns)
MARGIN = 0.1

KEYS = list(SPACE)
RAM_KEYS = list(RAM_SPACE)
SIM_FIELDS = KEYS + ['sim_pass', 'ipc', 'sim_s']
HW_FIELDS = ['fmax', 'wns', 'whs', 'lut', 'lutram', 'ff', 'bram', 'power_w',
             'dyn_w', 'levels', 'route_pct', 'secs', 'path']
SYNTH_FIELDS = KEYS + RAM_KEYS + HW_FIELDS
BIT_FIELDS = ['config', 'PLACE_DIRECTIVE', 'ROUTE_DIRECTIVE', 'margin'] + HW_FIELDS


def read_mk(path):
    vals = {}
    for line in open(path):
        m = re.match(r'^\s*(\w+)\s*=\s*(.*?)\s*$', line)
        if m:
            vals[m.group(1)] = m.group(2)
    return vals


def name(p):
    short = lambda k: k.lower().replace('_depth', '').replace('_ram_style', 'ram')
    return '_'.join('%s%s' % (short(k), p[k]) for k in sorted(p))


def sim_points():
    return [dict(zip(KEYS, v)) for v in itertools.product(*SPACE.values())]


def synth_points(default):
    """Base, one dimension at a time, all-min..all-max, every RAM style pair."""
    sizes = {k: default[k] for k in KEYS}
    rams = {k: default[k] for k in RAM_KEYS}
    points = [dict(default)]
    for k, values in SPACE.items():
        for v in values:
            points.append(dict(default, **{k: v}))
    n = max(len(v) for v in SPACE.values())
    for i in range(n):
        points.append(dict(rams, **{k: v[min(i, len(v) - 1)] for k, v in SPACE.items()}))
    for v in itertools.product(*RAM_SPACE.values()):
        points.append(dict(sizes, **dict(zip(RAM_KEYS, v))))
    unique = []
    for p in points:
        if p not in unique:
            unique.append(p)
    return unique


def write_cfg(p, path):
    text = open(BASE_CFG).read()
    for k, v in p.items():
        text = re.sub(r'^(%s\s*=\s*).*$' % k, r'\g<1>%s' % v, text, flags=re.M)
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
        w = csv.DictWriter(f, fieldnames=fields, extrasaction='ignore')
        w.writeheader()
        w.writerows(rows)


def key_of(r, keys):
    return tuple(str(r[k]) for k in keys)


def hw_results(rc, d, util, timing, power, clock, period):
    """Area, timing and power from one synth or bitstream run."""
    if rc != 0:
        return {'fmax': 'FAIL'}
    if clock:
        m = re.search(r'^\s+%s\s+(-?[\d.]+)\s+\S+\s+\S+\s+\S+\s+(-?[\d.]+)' % clock,
                      open(timing).read(), re.M)
        wns, whs = (m.group(1), m.group(2)) if m else ('', '')
    else:
        wns = grab(timing, r'^\s+WNS\(ns\).*\n.*\n\s+(-?[\d.]+)')
        whs = grab(timing, r'^\s+WNS\(ns\).*\n.*\n\s+-?[\d.]+\s+\S+\s+\S+\s+\S+\s+(-?[\d.]+)')
    row = {
        'wns': wns, 'whs': whs,
        'fmax': '%.1f' % (1000.0 / (period - float(wns))) if wns else '',
        'lut': grab(util, r'^\| Slice LUTs\s*\|\s*(\d+)'),
        'lutram': grab(util, r'^\|\s+LUT as Memory\s*\|\s*(\d+)'),
        'ff': grab(util, r'^\| Slice Registers\s*\|\s*(\d+)'),
        'bram': grab(util, r'^\| Block RAM Tile\s*\|\s*([\d.]+)'),
        'power_w': grab(power, r'^\| Total On-Chip Power \(W\)\s*\|\s*([\d.]+)'),
        'dyn_w': grab(power, r'^\| Dynamic \(W\)\s*\|\s*([\d.]+)'),
        'route_pct': grab(timing, r'Data Path Delay:.*route [\d.]+ns \(([\d.]+)%\)'),
    }
    try:
        top = open(os.path.join(d, 'critical_paths.txt')).read().splitlines()[1].split()
        row['levels'], row['path'] = top[1], ' '.join(top[4:])
    except (OSError, IndexError):
        pass
    return row


def sim_one(p):
    n = name(p)
    cfg = os.path.join(OUT, n + '.mk')
    write_cfg(p, cfg)
    log = os.path.join(OUT, n + '_sim.log')
    _, secs = run(['make', 'cpu', 'sim', 'CONFIG=' + cfg], log)
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
    rc, secs = run(['make', 'cpu', 'synth', 'CONFIG=' + cfg], log)
    row = dict(p, secs=secs)
    row.update(hw_results(rc, SYNTH, os.path.join(SYNTH, 'utilization_impl.rpt'),
                          os.path.join(SYNTH, 'timing_impl.rpt'),
                          os.path.join(SYNTH, 'power.rpt'), None, period))
    if rc == 0:
        os.remove(log)
    os.remove(cfg)
    return row


def bit_one(config, place, route, period, prog, first):
    """First run builds the bitstream (platform synthesis), the rest is place and route only."""
    tag = '%s_%s' % (place, route)
    cfg = os.path.join(OUT, tag + '.mk')
    write_cfg(config, cfg)
    log = os.path.join(OUT, tag + '_impl.log')
    target = ['fpga', 'bitstream'] if first else ['impl']
    rc, secs = run(['make'] + target + ['CONFIG=' + cfg, 'PLACE_DIRECTIVE=' + place, 'ROUTE_DIRECTIVE=' + route], log)
    d = os.path.join(FPGA_OUT, 'bitstream') if first else os.path.join(FPGA_OUT, 'impl', tag)
    row = {'config': name(config), 'PLACE_DIRECTIVE': place, 'ROUTE_DIRECTIVE': route, 'secs': secs}
    row.update(hw_results(rc, d, os.path.join(d, 'utilization.rpt'), os.path.join(d, 'timing.rpt'),
                          os.path.join(d, 'power.rpt'), 'clk_out1_design_1_clk_wiz_0_0', period))
    try:
        row['margin'] = 'ok' if float(row['wns']) >= MARGIN else 'fragile'
    except (KeyError, ValueError):
        row['margin'] = 'fail'
    if rc == 0:
        os.remove(log)
    os.remove(cfg)
    return row


def estimate(p, default, synth):
    """Measured point, else per-dimension model."""
    full = dict(p, **{k: default[k] for k in RAM_KEYS})
    if key_of(full, KEYS + RAM_KEYS) in synth:
        return synth[key_of(full, KEYS + RAM_KEYS)], ''
    base = synth.get(key_of(default, KEYS + RAM_KEYS))
    if not base or not base.get('lut'):
        return None, ''
    one = lambda k: synth.get(key_of(dict(default, **{k: p[k]}), KEYS + RAM_KEYS))
    est = {}
    for f in ('lut', 'ff', 'lutram', 'power_w', 'dyn_w'):
        try:
            v = float(base[f])
            for k in KEYS:
                v += float(one(k)[f]) - float(base[f])
            est[f] = '%.0f' % v if f in ('lut', 'ff', 'lutram') else '%.3f' % v
        except (TypeError, ValueError, KeyError):
            est[f] = ''
    try:
        est['fmax'] = '%.1f' % min(float(one(k)['fmax']) for k in KEYS)
    except (TypeError, ValueError, KeyError):
        est['fmax'] = ''
    return est, '~'


def table(f, title, fields, rows):
    f.write('## %s\n\n| %s |\n|%s\n' % (title, ' | '.join(fields), '---|' * len(fields)))
    for r in rows:
        f.write('| ' + ' | '.join(str(r.get(k, '')) for k in fields) + ' |\n')
    f.write('\n')


def report(default, sims, synth, bits, period):
    rows = []
    for p in sim_points():
        s = sims.get(key_of(p, KEYS))
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
    rows.sort(key=lambda r: (r['sim_pass'].split('/')[0] == r['sim_pass'].split('/')[-1],
                             float(r['fmax']) if r['fmax'] else -1), reverse=True)
    sizes = {k: default[k] for k in KEYS}
    ram_rows = [r for r in synth.values() if all(str(r[k]) == str(sizes[k]) for k in KEYS)]
    bit_rows = sorted(bits.values(), key=lambda r: float(r['wns']) if r.get('wns') else -99,
                      reverse=True)
    with open(os.path.join(OUT, 'results.md'), 'w') as f:
        f.write('Base configuration: %s\n\n' % ', '.join('%s=%s' % kv for kv in default.items()))
        table(f, 'Sizes (IPC measured everywhere; hardware measured where est is empty, '
                 'estimated per dimension where est is ~; mips = ipc x fmax)', fields, rows)
        table(f, 'RAM styles at the base sizes (CPU out of context)',
              RAM_KEYS + ['fmax', 'wns', 'lut', 'lutram', 'ff', 'bram', 'power_w', 'path'], ram_rows)
        table(f, 'Place x route on the cpu.mk configuration (platform at %.0f MHz; margin: fragile below %.1f ns)'
              % (1000.0 / period, MARGIN),
              ['config', 'PLACE_DIRECTIVE', 'ROUTE_DIRECTIVE', 'wns', 'whs', 'margin', 'fmax',
               'lut', 'ff', 'bram', 'power_w', 'secs', 'path'], bit_rows)


def main():
    os.makedirs(OUT, exist_ok=True)
    base = read_mk(BASE_CFG)
    period = 1000.0 / float(read_mk(PLATFORM_MK)['CPU_CLK_MHZ'])
    default = {k: int(base[k]) for k in KEYS}
    default.update({k: base[k] for k in RAM_KEYS})
    tools = read_mk(TOOLS_MK)
    default.update({k: tools[k] for k in ('PLACE_DIRECTIVE', 'ROUTE_DIRECTIVE')})

    sim_csv = os.path.join(OUT, 'sim.csv')
    synth_csv = os.path.join(OUT, 'synth.csv')
    bit_csv = os.path.join(OUT, 'bitstream.csv')
    sims = {key_of(r, KEYS): r for r in load(sim_csv)}
    synth = {key_of(r, KEYS + RAM_KEYS): r for r in load(synth_csv)}
    bits = {(r['config'], r['PLACE_DIRECTIVE'], r['ROUTE_DIRECTIVE']): r for r in load(bit_csv)}

    todo = [p for p in sim_points() if key_of(p, KEYS) not in sims]
    for n, p in enumerate(todo, 1):
        print('[sim %d/%d] %s' % (n, len(todo), name(p)), flush=True)
        sims[key_of(p, KEYS)] = sim_one(p)
        save(sim_csv, SIM_FIELDS, list(sims.values()))

    todo = [p for p in synth_points(default) if key_of(p, KEYS + RAM_KEYS) not in synth]
    for n, p in enumerate(todo, 1):
        print('[synth %d/%d] %s' % (n, len(todo), name(p)), flush=True)
        synth[key_of(p, KEYS + RAM_KEYS)] = synth_one(p, period)
        save(synth_csv, SYNTH_FIELDS, list(synth.values()))

    # Place x route on the cpu.mk configuration, only on request
    if '--directives' in sys.argv:
        chosen = {k: default[k] for k in KEYS + RAM_KEYS}
        run([os.path.join(ROOT, 'tb', 'scripts', 'compile.sh'), 'full_test'],
            os.path.join(OUT, 'compile.log'))
        prog = os.path.join(PROGS, 'instr_mem.txt')
        pairs = [(p, r) for r in ROUTES for p in DIRECTIVES]
        todo = [pr for pr in pairs if (name(chosen),) + pr not in bits]
        built = any(k[0] == name(chosen) for k in bits) and \
            os.path.exists(os.path.join(FPGA_OUT, 'project', 'project.xpr'))
        for i, (p, r) in enumerate(todo, 1):
            print('[place x route %d/%d] %s %s' % (i, len(todo), p, r), flush=True)
            bits[(name(chosen), p, r)] = bit_one(chosen, p, r, period, prog, first=not built)
            built = True
            save(bit_csv, BIT_FIELDS, list(bits.values()))

    report(default, sims, synth, bits, period)
    print('Results: ' + os.path.join(OUT, 'results.md'))


if __name__ == '__main__':
    sys.exit(main())
