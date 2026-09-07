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
        REBUILD [/path/to/cscope.files]
        STATUS
        QUIT

    Each response is zero or more result lines followed by a lone
    ``--END--`` line.  A result line is either ``file:line:text`` (a hit)
    or is prefixed with ``OK ``, ``WARN `` or ``ERROR `` -- diagnostics
    that the client shows to the user.  Every command answers with
    ``--END--`` even when it fails, so the client never hangs.
"""

import argparse
import bisect
import mmap
import os
import re
import sys
import traceback


# ---------------------------------------------------------------------------
# Build
# ---------------------------------------------------------------------------

def build(files_list, base, log=None):
    """Concatenate the files listed in FILES_LIST into BASE.dat / BASE.idx.

    Returns (nfiles, nbytes, nskipped).  Raises on anything fatal, with a
    message that names the absolute paths involved -- in daemon mode that
    message is what gets reported back to Emacs, so it has to be enough to
    diagnose the failure without a traceback.

    Relative entries in FILES_LIST are resolved against the directory that
    holds it, not the process cwd, so a rebuild works no matter where the
    daemon was started.  The archive is written to temporary files and
    renamed into place: a failed rebuild leaves the previous (working)
    archive alone, and an mmap held on the old .dat stays valid instead of
    being truncated underneath the running daemon.
    """
    log = log or (lambda msg: print(msg, file=sys.stderr))
    files_list = os.path.abspath(files_list)

    if not os.path.isfile(files_list):
        raise FileNotFoundError(
            f"file list not found: {files_list} (cwd {os.getcwd()})")
    with open(files_list, encoding="utf-8", errors="replace") as fl:
        paths = [l.strip() for l in fl if l.strip()]
    if not paths:
        raise ValueError(f"file list is empty: {files_list}")

    root = os.path.dirname(files_list)
    base = os.path.abspath(base)
    outdir = os.path.dirname(base) or "."
    if not os.path.isdir(outdir):
        raise NotADirectoryError(f"archive directory does not exist: {outdir}")
    if not os.access(outdir, os.W_OK):
        raise PermissionError(f"archive directory is not writable: {outdir}")

    tmp_dat, tmp_idx = base + ".dat.tmp", base + ".idx.tmp"
    off, n, skip = 0, 0, 0
    try:
        with open(tmp_dat, "wb") as dat, \
             open(tmp_idx, "w", encoding="utf-8") as idx:
            for p in paths:
                src = p if os.path.isabs(p) else os.path.join(root, p)
                try:
                    with open(src, "rb") as fh:
                        data = fh.read()
                except OSError as e:
                    if skip < 20:          # don't drown the response
                        log(f"WARN skip {src}: {e.strerror}")
                    skip += 1
                    continue
                if data and not data.endswith(b"\n"):
                    data += b"\n"
                idx.write(f"{off}\t{p}\n")
                dat.write(data)
                off += len(data)
                n += 1
        if skip > 20:
            log(f"WARN ... and {skip - 20} more unreadable files")
        if n == 0:
            raise ValueError(
                f"none of the {len(paths)} files listed in {files_list} "
                f"could be read (resolved against {root})")
        os.replace(tmp_dat, base + ".dat")
        os.replace(tmp_idx, base + ".idx")
    except BaseException:
        for tmp in (tmp_dat, tmp_idx):
            try:
                os.unlink(tmp)
            except OSError:
                pass
        raise

    return n, off, skip


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


def _describe(path):
    try:
        return f"{path}: {os.path.getsize(path)} bytes"
    except OSError as e:
        return f"{path}: {e.strerror}"


def serve(base):
    engine = None
    try:
        if os.path.exists(base + ".dat") and os.path.exists(base + ".idx"):
            engine = Engine(base)
    except Exception as e:                       # noqa: BLE001 - reported, not raised
        print(f"ERROR cannot open archive {os.path.abspath(base)}: "
              f"{type(e).__name__}: {e}", flush=True)
    print("READY", flush=True)

    for raw in sys.stdin:
        line = raw.strip()
        if not line:
            continue
        parts = line.split(None, 1)
        cmd = parts[0].upper()
        arg = parts[1].strip() if len(parts) > 1 else ""

        if cmd == "QUIT":
            break

        # Every command answers with --END--, including the failures: the
        # client blocks on that sentinel, so an uncaught exception here used
        # to kill the daemon and leave Emacs waiting forever with nothing to
        # show.  Report the error in-band instead and stay alive.
        try:
            if cmd == "REBUILD":
                n, nbytes, skip = build(arg or "cscope.files", base,
                                        log=lambda m: print(m, flush=True))
                if engine:
                    engine.reload()
                else:
                    engine = Engine(base)
                msg = (f"OK rebuilt {os.path.abspath(base)}.dat: "
                       f"{n} files, {nbytes} bytes")
                if skip:
                    msg += f", {skip} unreadable"
                print(msg, flush=True)

            elif cmd == "SEARCH":
                if not engine:
                    print("ERROR no archive loaded -- rebuild it first "
                          f"(looked for {os.path.abspath(base)}.dat)",
                          flush=True)
                else:
                    pattern, ic, fx = _parse_search_args(arg)
                    if not pattern:
                        print("ERROR empty pattern", flush=True)
                    else:
                        for p, ln, txt in engine.search(pattern, ic, fx):
                            print(f"{p}:{ln}:{txt}", flush=True)

            elif cmd == "STATUS":
                flist = os.path.abspath(arg or "cscope.files")
                print(f"OK cwd          {os.getcwd()}", flush=True)
                print(f"OK python       {sys.version.split()[0]} "
                      f"({sys.executable})", flush=True)
                print(f"OK file list    {flist}"
                      f"{'' if os.path.isfile(flist) else '   [MISSING]'}",
                      flush=True)
                print(f"OK archive      {_describe(os.path.abspath(base) + '.dat')}",
                      flush=True)
                print(f"OK index        {_describe(os.path.abspath(base) + '.idx')}",
                      flush=True)
                print(f"OK indexed      {len(engine.paths) if engine else 0} files",
                      flush=True)

            else:
                print(f"ERROR unknown command: {cmd}", flush=True)

        except Exception as e:                   # noqa: BLE001 - reported, not raised
            print(f"ERROR {cmd}: {type(e).__name__}: {e}", flush=True)
            traceback.print_exc(file=sys.stderr)

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
    try:
        eng = Engine(base)
    except OSError as e:
        sys.exit(f"search: cannot open {os.path.abspath(base)}.dat/.idx: "
                 f"{e.strerror}")
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
        try:
            n, nbytes, skip = build(args.files, args.base)
        except Exception as e:                   # noqa: BLE001 - message, not traceback
            sys.exit(f"build: {e}")
        print(f"{os.path.abspath(args.base)}.dat: {n} files, {nbytes} bytes"
              + (f", {skip} unreadable" if skip else ""), file=sys.stderr)
    elif args.cmd == "serve":
        serve(args.base)
    elif args.cmd == "search":
        cmd_search(args.base, extra)


if __name__ == "__main__":
    main()
