#!/usr/bin/env python3
"""Summarise the gfortran warnings in a build log, for the PR build.

Compiler warnings are reported, not enforced: the tree still carries ~100
legacy -Wall -Wextra warnings (see doc/LINT_NOTES.md), so this never fails.
It writes a count per warning flag, and the full list, to the job summary,
and raises inline annotations only for warnings in files the PR changes -
GitHub keeps just 10 warning annotations per step, and the legacy ones
would otherwise crowd out the one a PR introduced.

    gfortran_warnings.py BUILD_LOG SOURCE_ROOT [CHANGED_FILES]

CHANGED_FILES lists repo-relative paths, one per line; without it no
annotations are raised.
"""

import collections
import os
import re
import sys

# The front end puts the location on a line of its own and the message a few
# lines later; the middle end (-Wmaybe-uninitialized, sometimes) uses one line.
LOCATION = re.compile(r"^(\S+\.[fF]\w*):(\d+):(\d+):$")
MESSAGE = re.compile(r"^Warning: (.*)$")
ONE_LINE = re.compile(r"^(\S+\.[fF]\w*):(\d+):(\d+): [Ww]arning: (.*)$")
FLAG = re.compile(r"\[(-W[\w=-]+)\]\s*$")


def parse(log_lines, root):
    warnings = []
    location = None
    for line in log_lines:
        line = line.rstrip("\n")
        m = ONE_LINE.match(line)
        if m:
            warnings.append((*m.group(1, 2, 3), m.group(4)))
            location = None
            continue
        m = LOCATION.match(line)
        if m:
            location = m.group(1, 2, 3)
            continue
        m = MESSAGE.match(line)
        if m and location:
            warnings.append((*location, m.group(1)))
            location = None

    seen = set()
    unique = []
    for path, row, col, text in warnings:
        path = os.path.relpath(os.path.realpath(path), root)
        key = (path, row, col, text)
        if key not in seen:
            seen.add(key)
            unique.append(key)
    return unique


def escape(text):
    # Workflow-command data escaping, so a message cannot end the command.
    return text.replace("%", "%25").replace("\r", "%0D").replace("\n", "%0A")


def main():
    log, root = sys.argv[1], os.path.realpath(sys.argv[2])
    changed = set()
    if len(sys.argv) > 3:
        with open(sys.argv[3]) as f:
            changed = {line.strip() for line in f if line.strip()}

    with open(log, errors="replace") as f:
        warnings = parse(f, root)

    by_flag = collections.Counter()
    for *_, text in warnings:
        m = FLAG.search(text)
        by_flag[m.group(1) if m else "(no flag)"] += 1

    annotated = 0
    for path, row, col, text in warnings:
        if path in changed:
            annotated += 1
            print(f"::warning file={path},line={row},col={col},"
                  f"title=gfortran::{escape(text)}")

    out = [f"### gfortran warnings: {len(warnings)}", ""]
    if changed:
        out += [f"{annotated} in files this PR changes, annotated inline.", ""]
    if warnings:
        out += ["| Flag | Count |", "|---|---:|"]
        out += [f"| `{flag}` | {n} |" for flag, n in by_flag.most_common()]
        out += ["", "<details><summary>All warnings</summary>", "", "```"]
        out += [f"{p}:{r}:{c}: {t}" for p, r, c, t in sorted(warnings)]
        out += ["```", "</details>"]
    summary = "\n".join(out) + "\n"

    print(summary)
    if os.environ.get("GITHUB_STEP_SUMMARY"):
        with open(os.environ["GITHUB_STEP_SUMMARY"], "a") as f:
            f.write(summary)


if __name__ == "__main__":
    main()
