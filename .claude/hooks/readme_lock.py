#!/usr/bin/env python3
# README lock: Claude may change only numbers
import json, os, re, sys

def norm(s):
    return re.sub(r'\d+', '0', s)

def deny(msg):
    print(f'README locked: {msg}. Do not touch any README; report numbers in the chat.', file=sys.stderr)
    sys.exit(2)

data = json.load(sys.stdin)
tool, inp = data.get('tool_name', ''), data.get('tool_input', {})

if tool == 'Bash':
    cmd = inp.get('command', '')
    if re.search(r'README\.md', cmd) and re.search(r'sed\s+-i|>|\btee\b|\bmv\b|\bcp\b|\brm\b|perl\s+-[a-z]*i', cmd):
        deny('shell writes to README are blocked, use Edit')
    sys.exit(0)

path = inp.get('file_path', '')
if os.path.basename(path) != 'README.md':
    sys.exit(0)

deny('README files are maintained by the user only')
