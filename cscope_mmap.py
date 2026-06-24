#!/usr/bin/env python3
"""mmap-based code search daemon.

Concatenates source files from cscope.files into a flat file with a
byte-offset index.  Searches the file via mmap.  Runs as a
stdin/stdout daemon so Emacs can start it once and query repeatedly.

Standalone:
    cscope_mmap.py build  [-f cscope.files] [-b BASE]
    cscope_mmap.py search [-b BASE] [-i] [-F] PATTERN

Daemon (started by Emacs):
    cscope_mmap.py serve  [-b BASE]

    Commands on stdin (one per line):
        SEARCH [-i] [-F] PATTERN
        REBUILD [cscope.files]
        QUIT

    Each response is zero or more ``file:line:text`` lines followed by
    a lone ``--END--`` line.
"""

import argparse
import bisect
import mmap
import os
import re
import sys


# ---------------------------------------------------------------------------
# Build
# ---------------------------------------------------------------------------

def build(files_list, base):
    with open(files_list, encoding="utf-8", errors="replace") as fl:
        paths = [l.strip() for l in fl if l.strip()]
    if not paths:
        print(f"{files_list}: empty", file=sys.stderr)
        return False

    off, n, skip = 0, 0, 0
    with open(base + ".dat", "wb") as dat, \
         open(base + ".idx", "w", encoding="utf-8") as idx:
        for p in paths:
            try:
                data = open(p, "rb").read()
            except OSError as e:
                print(f"skip {p}: {e}", file=sys.stderr)
                skip += 1
                continue
            if data and not data.endswith(b"\n"):
                data += b"\n"
            idx.write(f"{off}\t{p}\n")
            dat.write(data)
            off += len(data)
            n += 1

    print(f"{base}.dat: {n} files, {off} bytes", file=sys.stderr)
    if skip:
        print(f"({skip} skipped)", file=sys.stderr)
    return True


# ---------------------------------------------------------------------------
# Search engine
# ---------------------------------------------------------------------------

class Engine:
    __slots__ = ("base", "mm", "fd", "starts", "paths")

    def __init__(self, base):
        self.base = base
        self.mm = None
        self.fd = None
        self.starts = []
        self.paths = []
        self._load()

    def _load(self):
        self.close()
        self.starts, self.paths = [], []
        with open(self.base + ".idx", encoding="utf-8") as f:
            for ln in f:
                ln = ln.rstrip("\n")
                if ln:
                    off, path = ln.split("\t", 1)
                    self.starts.append(int(off))
                    self.paths.append(path)
        self.fd = os.open(self.base + ".dat", os.O_RDONLY)
        sz = os.fstat(self.fd).st_size
        self.mm = mmap.mmap(self.fd, 0, access=mmap.ACCESS_READ) if sz else None

    def close(self):
        if self.mm:
            self.mm.close()
            self.mm = None
        if self.fd is not None:
            os.close(self.fd)
            self.fd = None

    def reload(self):
        self._load()

    def _locate(self, pos):
        i = bisect.bisect_right(self.starts, pos) - 1
        if i < 0:
            return None, 0
        local = self.mm[self.starts[i]:pos].count(b"\n") + 1
        return self.paths[i], local

    def _line_at(self, pos):
        s = self.mm.rfind(b"\n", 0, pos)
        s = 0 if s < 0 else s + 1
        e = self.mm.find(b"\n", pos)
        if e < 0:
            e = len(self.mm)
        return self.mm[s:e].decode("utf-8", errors="replace")

    def search(self, pattern, ignore_case=False, fixed=False):
        if not self.mm:
            return
        flags = re.IGNORECASE if ignore_case else 0
        raw = pattern.encode("utf-8", errors="replace")
        pat = re.compile(re.escape(raw) if fixed else raw, flags)
        seen = set()
        for m in pat.finditer(self.mm):
            ls = self.mm.rfind(b"\n", 0, m.start())
            ls = 0 if ls < 0 else ls + 1
            if ls in seen:
                continue
            seen.add(ls)
            p, ln = self._locate(m.start())
            if p:
                yield p, ln, self._line_at(m.start())


# ---------------------------------------------------------------------------
# Daemon
# ---------------------------------------------------------------------------

def _parse_search_args(rest):
    ic, fx = False, False
    tokens = rest.split()
    i = 0
    while i < len(tokens):
        if tokens[i] == "-i":
            ic = True
        elif tokens[i] == "-F":
            fx = True
        else:
            break
        i += 1
    return " ".join(tokens[i:]), ic, fx


def serve(base):
    engine = None
    if os.path.exists(base + ".dat") and os.path.exists(base + ".idx"):
        engine = Engine(base)
    print("READY", flush=True)

    for raw in sys.stdin:
        line = raw.strip()
        if not line:
            continue
        parts = line.split(None, 1)
        cmd = parts[0].upper()

        if cmd == "QUIT":
            break

        elif cmd == "REBUILD":
            flist = parts[1].strip() if len(parts) > 1 else "cscope.files"
            build(flist, base)
            if engine:
                engine.reload()
            else:
                engine = Engine(base)
            print("--END--", flush=True)

        elif cmd == "SEARCH":
            if not engine:
                print("ERROR no archive — run REBUILD first", flush=True)
                print("--END--", flush=True)
                continue
            pattern, ic, fx = _parse_search_args(parts[1] if len(parts) > 1 else "")
            if not pattern:
                print("ERROR empty pattern", flush=True)
                print("--END--", flush=True)
                continue
            try:
                for p, ln, txt in engine.search(pattern, ic, fx):
                    print(f"{p}:{ln}:{txt}", flush=True)
            except re.error as e:
                print(f"ERROR {e}", flush=True)
            print("--END--", flush=True)

        else:
            print(f"ERROR unknown command: {cmd}", flush=True)
            print("--END--", flush=True)

    if engine:
        engine.close()


# ---------------------------------------------------------------------------
# One-shot CLI search
# ---------------------------------------------------------------------------

def cmd_search(base, args):
    pattern, ic, fx = _parse_search_args(" ".join(args))
    if not pattern:
        sys.exit("search: PATTERN required")
    eng = Engine(base)
    hits = 0
    for p, ln, txt in eng.search(pattern, ic, fx):
        print(f"{p}:{ln}:{txt}")
        hits += 1
    eng.close()
    if not hits:
        sys.exit(1)


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    p = argparse.ArgumentParser(
        description="mmap code search with Emacs daemon mode")
    sub = p.add_subparsers(dest="cmd", required=True)

    b = sub.add_parser("build", help="concatenate files into flat archive + index")
    b.add_argument("-f", "--files", default="cscope.files")
    b.add_argument("-b", "--base", default="cscope_archive")

    s = sub.add_parser("serve", help="stdin/stdout daemon for Emacs")
    s.add_argument("-b", "--base", default="cscope_archive")

    q = sub.add_parser("search", help="one-shot search")
    q.add_argument("-b", "--base", default="cscope_archive")

    args, extra = p.parse_known_args()
    if args.cmd == "build":
        build(args.files, args.base)
    elif args.cmd == "serve":
        serve(args.base)
    elif args.cmd == "search":
        cmd_search(args.base, extra)


if __name__ == "__main__":
    main()
