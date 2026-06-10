#!/usr/bin/env python3
"""Concatenate every file in cscope.files into one gzipped archive plus a
line-number -> file lookup index, then search it with zgrep.

The archive (`ARCHIVE.txt.gz`) is just every source file appended one after
another.  The index (`ARCHIVE.idx`) records, for each source file, the global
line number in the concatenated stream at which that file begins.  Given a
global line number from `zgrep -n`, a binary search over the index recovers
which original file it came from and the line number within that file.

Usage:
    cscope_archive.py build  [-f cscope.files] [-a ARCHIVE]
    cscope_archive.py search [-a ARCHIVE] [grep-opts...] PATTERN

Examples:
    cscope_archive.py build
    cscope_archive.py build -f /proj/cscope.files -a /proj/code
    cscope_archive.py search foo_bar
    cscope_archive.py search -i -E 'foo|bar'
"""

import argparse
import bisect
import os
import subprocess
import sys
from gzip import GzipFile


def archive_paths(base):
    """Return (gzip_archive, index) paths derived from a base name."""
    return base + ".txt.gz", base + ".idx"


def build(files_list, base):
    archive, index = archive_paths(base)

    with open(files_list, encoding="utf-8", errors="replace") as fl:
        paths = [ln.strip() for ln in fl if ln.strip()]
    if not paths:
        sys.exit(f"{files_list}: no file paths found")

    start_line = 1            # global line number where the current file starts
    written = 0
    missing = 0

    # Write the concatenated text straight into the gzip stream and the index
    # alongside it.  No giant intermediate plain-text file is kept on disk.
    with GzipFile(archive, "wb") as gz, \
         open(index, "w", encoding="utf-8") as idx:
        for path in paths:
            try:
                with open(path, "rb") as f:
                    data = f.read()
            except OSError as e:
                print(f"skip {path}: {e}", file=sys.stderr)
                missing += 1
                continue

            # Guarantee a clean line boundary so the next file can't be glued
            # onto this file's last line and corrupt the line numbering.
            if data and not data.endswith(b"\n"):
                data += b"\n"

            nlines = data.count(b"\n")
            # start_line <TAB> path  (path may legitimately contain spaces)
            idx.write(f"{start_line}\t{path}\n")
            gz.write(data)
            start_line += nlines
            written += 1

    print(f"archive: {archive}  ({written} files, {start_line - 1} lines)")
    print(f"index:   {index}")
    if missing:
        print(f"({missing} file(s) skipped — see stderr)", file=sys.stderr)


def load_index(index):
    """Return parallel lists (starts, paths) sorted by global start line."""
    starts, paths = [], []
    with open(index, encoding="utf-8") as f:
        for ln in f:
            ln = ln.rstrip("\n")
            if not ln:
                continue
            start_str, path = ln.split("\t", 1)
            starts.append(int(start_str))
            paths.append(path)
    # The index is written in ascending order already, but don't rely on it.
    order = sorted(range(len(starts)), key=lambda i: starts[i])
    starts = [starts[i] for i in order]
    paths = [paths[i] for i in order]
    return starts, paths


def locate(starts, paths, global_line):
    """Map a global line number to (file, local_line)."""
    # Rightmost file whose start <= global_line.
    i = bisect.bisect_right(starts, global_line) - 1
    if i < 0:
        return None, None
    local = global_line - starts[i] + 1
    return paths[i], local


def search(base, grep_args):
    archive, index = archive_paths(base)
    if not os.path.exists(archive):
        sys.exit(f"{archive}: not found — run `build` first")
    if not os.path.exists(index):
        sys.exit(f"{index}: not found — run `build` first")

    starts, paths = load_index(index)

    # -n is forced so we always get a global line number to translate.
    cmd = ["zgrep", "-n"] + grep_args + [archive]
    proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, text=True,
                            errors="replace")

    hits = 0
    for line in proc.stdout:
        line = line.rstrip("\n")
        # zgrep -n output: "<global_lineno>:<matched text>"
        num, sep, text = line.partition(":")
        if not sep or not num.isdigit():
            continue
        path, local = locate(starts, paths, int(num))
        if path is None:
            continue
        print(f"{path}:{local}:{text}")
        hits += 1

    proc.wait()
    # zgrep exit code 1 just means "no matches"; >1 is a real error.
    if proc.returncode > 1:
        sys.exit(proc.returncode)
    if hits == 0:
        sys.exit(1)


def main():
    p = argparse.ArgumentParser(
        description="Build a gzipped concatenation of cscope.files and search "
                    "it with zgrep, mapping hits back to original files.")
    sub = p.add_subparsers(dest="cmd", required=True)

    b = sub.add_parser("build", help="build the gzipped archive and index")
    b.add_argument("-f", "--files", default="cscope.files",
                   help="list of files to archive (default: cscope.files)")
    b.add_argument("-a", "--archive", default="cscope_archive",
                   help="archive base name (default: cscope_archive)")

    s = sub.add_parser("search", help="zgrep the archive; report file:line")
    s.add_argument("-a", "--archive", default="cscope_archive",
                   help="archive base name (default: cscope_archive)")
    # Everything else (grep flags + PATTERN) is passed straight through to zgrep.
    # parse_known_args is used so leading-dash grep flags aren't mis-parsed as
    # options of this script.

    args, extra = p.parse_known_args()
    if args.cmd == "build":
        if extra:
            sys.exit(f"build: unexpected arguments: {' '.join(extra)}")
        build(args.files, args.archive)
    elif args.cmd == "search":
        if not extra:
            sys.exit("search: a PATTERN (and optional grep flags) is required")
        search(args.archive, extra)


if __name__ == "__main__":
    main()
