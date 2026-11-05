#!/usr/bin/env python3
# Git lock: commits and pushes only through the verification hooks
import json, re, subprocess, sys

def deny(msg):
    print(f'Git locked: {msg}. Use make commit MSG="...".', file=sys.stderr)
    sys.exit(2)

data = json.load(sys.stdin)
if data.get('tool_name') != 'Bash':
    sys.exit(0)
cmd = data.get('tool_input', {}).get('command', '')
for part in re.split(r'&&|\|\||;|\|', cmd):
    if not re.search(r'\bgit\b', part):
        continue
    if re.search(r'--no-verify|core\.hooksPath', part):
        deny('hook bypass or hooksPath change')
    if re.search(r'\bgit\b.*\bcommit\b.*\s-[a-zA-Z]*n', part):
        deny('git commit -n skips the hooks')
    if re.search(r'\bgit\b.*\b(commit|push)\b', part):
        path = subprocess.run(['git', 'config', 'core.hooksPath'], capture_output=True, text=True).stdout.strip()
        if path != 'tools/git-hooks':
            deny('git hooks not active (make check sets core.hooksPath)')
sys.exit(0)
