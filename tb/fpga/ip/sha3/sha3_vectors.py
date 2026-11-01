#!/usr/bin/env python3
"""SHA3-256 vectors for sha3_ip_tb: padded 64-bit lanes per block and expected digest lanes."""
import hashlib
import random
import sys
from pathlib import Path

RATE = 136
LENGTHS = [0, 1, 3, 8, 135, 136, 137, 200, 271, 272, 500, 1000]


def pad(msg):
    padded = bytearray(msg) + b"\x06" + bytes((-len(msg) - 1) % RATE)
    padded[-1] |= 0x80
    return bytes(padded)


def lanes(data):
    return [int.from_bytes(data[i:i + 8], "little") for i in range(0, len(data), 8)]


def main():
    out = Path(sys.argv[1])
    out.mkdir(parents=True, exist_ok=True)
    rng = random.Random(1)
    with open(out / "in.txt", "w") as fin, open(out / "exp.txt", "w") as fexp:
        for n in LENGTHS:
            msg = bytes(rng.randrange(256) for _ in range(n))
            words = lanes(pad(msg))
            fin.write(f"{len(words) // 17}\n")
            fin.writelines(f"{w:016x}\n" for w in words)
            fexp.writelines(f"{w:016x}\n" for w in lanes(hashlib.sha3_256(msg).digest()))
    print(f"{len(LENGTHS)} messages -> {out}")


if __name__ == "__main__":
    main()
