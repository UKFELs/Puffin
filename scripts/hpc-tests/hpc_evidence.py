#!/usr/bin/env python3
"""Evidence that the slow and big tests passed on an HPC machine, for a PR.

The big 3D test needs ~12 GB and ~9 minutes on 6 ranks, which no hosted CI
runner offers, so it is run by hand elsewhere and the result is posted to the
pull request as a comment. CI then checks that comment (UKFELs/Puffin#116).
There are three steps, normally in this order:

    collect  BUILD_DIR SOURCE_DIR OUT_DIR
        Run on the HPC machine by run_hpc_tests.sh once ctest has finished.
        Reads the ctest JUnit report and log, and writes OUT_DIR/evidence.json
        and OUT_DIR/comment.md. Needs nothing beyond the Python standard
        library and git, so it works on a compute node with no network.

    submit   OUT_DIR --pr N [--repo OWNER/NAME]
        Run anywhere `gh` is logged in - the HPC login node, or a laptop the
        evidence directory was copied to. Checks the evidence is for the PR's
        current head commit, posts comment.md to the PR, and re-runs the PR's
        failed `hpc-evidence` check so it picks the comment up.

    verify   --pr N --head-sha SHA [--repo OWNER/NAME]
        Run by .github/workflows/hpc-evidence.yml, from a checkout of SHA.
        Finds the newest evidence comment for SHA left by someone with write
        access to the repository, and fails unless it shows every required
        test passing on a clean tree of SHA, with the big test's field and
        beam reductions inside tolerance of the golden constants *in that
        commit's own test source*.

This is evidence, not proof: nothing here can stop a collaborator posting a
fabricated comment. What it does guarantee is that the result is tied to the
exact commit under review - a push after the HPC run makes the check fail
again - and that the numbers reported are the ones this commit's test expects.
"""

import argparse
import datetime
import json
import os
import platform
import re
import socket
import subprocess
import sys
import xml.etree.ElementTree as ET

SCHEMA = 1

# The comment carries this marker, so verify can tell evidence from discussion.
MARKER = "<!-- puffin-hpc-evidence v%d -->" % SCHEMA

# Tests that must have run and passed. The slow test also runs in hosted CI on
# PRs into master; it is repeated here because it costs ~30 s and checks it on
# a second platform.
REQUIRED_TESTS = ["puffin_e2e_tests_3d_slow", "puffin_e2e_tests_3d_big"]

BIG_TEST_SOURCE = "test/testMPIIntegration3DBig.pf"

# Comments from anyone else are ignored, however well formed.
TRUSTED_ASSOCIATIONS = {"OWNER", "MEMBER", "COLLABORATOR"}

# A golden constant in the .pf, and the line the test prints for it, are both
# NAME = VALUE, with an optional _WP / _ip kind suffix on the source side.
GOLDEN = re.compile(r"\b(SHCSE_BIG_[A-Z0-9_]+)\s*=\s*([-+]?[0-9][0-9.]*(?:[EeDd][-+]?[0-9]+)?)")
REL_TOL = re.compile(r"\brelTol\s*=\s*([0-9.]+[EeDd][-+]?[0-9]+)")
NPES = re.compile(r"@mpitest\(npes=\[([0-9]+)\]\)")

# Reductions the big test computes and prints. The mesh and step constants are
# in the source too, but are asserted exactly by the test itself and not
# printed, so they are not part of the evidence.
REDUCTIONS = [
    "SHCSE_BIG_FIELD_SUMABSR", "SHCSE_BIG_FIELD_SUMABSI",
    "SHCSE_BIG_FIELD_INTENS", "SHCSE_BIG_FIELD_MAXAMP",
    "SHCSE_BIG_RMS_X", "SHCSE_BIG_RMS_Y", "SHCSE_BIG_RMS_Z2",
    "SHCSE_BIG_RMS_PX", "SHCSE_BIG_RMS_PY", "SHCSE_BIG_RMS_GAM",
    "SHCSE_BIG_SUM_CHI",
]
COUNTS = ["SHCSE_BIG_NMPS"]

LOG_TAIL_LINES = 60


def fortran_float(text):
    return float(text.replace("D", "E").replace("d", "e"))


def run(cmd, cwd=None, check=True):
    out = subprocess.run(cmd, cwd=cwd, stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE, universal_newlines=True)
    if check and out.returncode != 0:
        sys.exit("error: %s failed:\n%s" % (" ".join(cmd), out.stderr.strip()))
    return out.stdout.strip()


def golden_constants(pf_source):
    """The reference values, tolerance and rank count in the big test's source."""
    values = {}
    for name, value in GOLDEN.findall(pf_source):
        if name in REDUCTIONS:
            values[name] = fortran_float(value)
        elif name in COUNTS:
            values[name] = int(value)
    tol = REL_TOL.search(pf_source)
    npes = NPES.search(pf_source)
    return values, (fortran_float(tol.group(1)) if tol else None), \
        (int(npes.group(1)) if npes else None)


def printed_values(log_text):
    """The reductions the big test printed, the last occurrence of each."""
    values = {}
    for name, value in GOLDEN.findall(log_text):
        if name in REDUCTIONS:
            values[name] = fortran_float(value)
        elif name in COUNTS:
            values[name] = int(float(value))
    return values


def cmake_cache(build_dir):
    cache = {}
    try:
        with open(os.path.join(build_dir, "CMakeCache.txt")) as f:
            for line in f:
                m = re.match(r"^([A-Za-z0-9_]+):[A-Z]+=(.*)$", line.rstrip("\n"))
                if m:
                    cache[m.group(1)] = m.group(2)
    except OSError:
        pass
    return cache


def junit_results(path):
    """{test name: {status, time}} from a ctest --output-junit report."""
    results = {}
    for case in ET.parse(path).getroot().iter("testcase"):
        if case.find("failure") is not None or case.find("error") is not None:
            status = "failed"
        elif case.find("skipped") is not None or case.get("status") in ("notrun", "disabled"):
            status = "skipped"
        else:
            status = "passed"
        results[case.get("name")] = {"status": status,
                                     "time_s": round(float(case.get("time") or 0), 1)}
    return results


def first_line(cmd):
    try:
        out = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                             universal_newlines=True, timeout=30)
        lines = [l for l in out.stdout.splitlines() if l.strip()]
        return lines[0].strip() if lines else ""
    except (OSError, subprocess.SubprocessError):
        return ""


def check_evidence(ev, head_sha, pf_source):
    """Everything wrong with an evidence record, as a list of messages."""
    problems = []
    if ev.get("schema") != SCHEMA:
        problems.append("evidence schema is %r, expected %d" % (ev.get("schema"), SCHEMA))
    if head_sha is not None and ev.get("head_sha") != head_sha:
        problems.append("evidence is for commit %s, not the PR head %s"
                        % (ev.get("head_sha"), head_sha))
    if not ev.get("tree_clean"):
        problems.append("the source tree had local modifications when the tests were built")
    if ev.get("ctest_exit_code") != 0:
        problems.append("ctest exited with code %r" % ev.get("ctest_exit_code"))

    tests = ev.get("tests", {})
    for name in REQUIRED_TESTS:
        status = tests.get(name, {}).get("status")
        if status != "passed":
            problems.append("%s: %s" % (name, status or "did not run"))

    expected, tol, npes = golden_constants(pf_source)
    if tol is None or npes is None:
        problems.append("could not read relTol and npes from %s" % BIG_TEST_SOURCE)
        return problems
    if ev.get("big_test_pes") != npes:
        problems.append("run used %r ranks, %s declares npes=[%d]"
                        % (ev.get("big_test_pes"), BIG_TEST_SOURCE, npes))

    got = ev.get("big_test_values", {})
    for name in REDUCTIONS + COUNTS:
        if name not in expected:
            problems.append("%s not found in %s" % (name, BIG_TEST_SOURCE))
        elif name not in got:
            problems.append("%s missing from the evidence" % name)
        elif name in COUNTS:
            if got[name] != expected[name]:
                problems.append("%s = %d, expected %d" % (name, got[name], expected[name]))
        elif abs(got[name] - expected[name]) > tol * abs(expected[name]):
            problems.append("%s = %.17e, expected %.17e (relTol %.1e)"
                            % (name, got[name], expected[name], tol))
    return problems


# --- collect ---------------------------------------------------------------

def collect(args):
    build, source, out = args.build_dir, args.source_dir, args.out_dir
    os.makedirs(out, exist_ok=True)

    head = run(["git", "rev-parse", "HEAD"], cwd=source)
    dirty = run(["git", "status", "--porcelain", "--untracked-files=no"], cwd=source)
    cache = cmake_cache(build)

    with open(args.log, errors="replace") as f:
        log_text = f.read()
    tests = junit_results(args.junit)

    ev = {
        "schema": SCHEMA,
        "head_sha": head,
        "tree_clean": not dirty,
        "branch": run(["git", "rev-parse", "--abbrev-ref", "HEAD"], cwd=source, check=False),
        "finished_utc": datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "host": socket.gethostname(),
        "platform": platform.platform(),
        "fortran_compiler": "%s %s" % (cache.get("CMAKE_Fortran_COMPILER_ID", "?"),
                                       cache.get("CMAKE_Fortran_COMPILER_VERSION", "?")),
        "mpiexec": cache.get("MPIEXEC_EXECUTABLE", ""),
        "mpi_version": first_line([cache.get("MPIEXEC_EXECUTABLE") or "mpiexec", "--version"]),
        "cmake_build_type": cache.get("CMAKE_BUILD_TYPE", ""),
        "big_test_pes": int(cache["PUFFIN_BIG_TEST_PES"]) if cache.get("PUFFIN_BIG_TEST_PES") else None,
        "omp_num_threads": os.environ.get("OMP_NUM_THREADS", ""),
        "ctest_exit_code": args.ctest_exit_code,
        "tests": tests,
        "big_test_values": printed_values(log_text),
    }
    if args.notes:
        ev["notes"] = args.notes

    with open(os.path.join(out, "evidence.json"), "w") as f:
        json.dump(ev, f, indent=2, sort_keys=True)
        f.write("\n")

    with open(os.path.join(source, BIG_TEST_SOURCE)) as f:
        problems = check_evidence(ev, None, f.read())

    tail = log_text.splitlines()[-LOG_TAIL_LINES:]
    with open(os.path.join(out, "comment.md"), "w") as f:
        f.write(render_comment(ev, problems, tail))

    print("Wrote %s/evidence.json and %s/comment.md" % (out, out))
    if problems:
        print("\nThis evidence will NOT pass verification:")
        for p in problems:
            print("  - " + p)
        return 1
    print("All required tests passed; post it with:\n"
          "  %s submit %s --pr <N>" % (sys.argv[0], out))
    return 0


def render_comment(ev, problems, log_tail):
    ok = not problems
    rows = "\n".join("| `%s` | %s | %.0f s |" % (name, t["status"], t["time_s"])
                     for name, t in sorted(ev["tests"].items()))
    lines = [
        MARKER,
        "### %s HPC slow/big tests for %s" % ("✅" if ok else "❌", ev["head_sha"]),
        "",
        "Ran on `%s` (%s), %s, %d ranks, finished %s."
        % (ev["host"], ev["platform"], ev["fortran_compiler"],
           ev["big_test_pes"] or 0, ev["finished_utc"]),
        "",
        "| Test | Result | Time |",
        "|---|---|---|",
        rows,
        "",
    ]
    if ev.get("notes"):
        lines += [ev["notes"], ""]
    if problems:
        lines += ["**Problems:**", ""] + ["- " + p for p in problems] + [""]
    lines += [
        "Checked by the `hpc-evidence` workflow; see `scripts/hpc-tests/README.md`.",
        "",
        "<details><summary>Evidence record</summary>",
        "",
        "```json",
        json.dumps(ev, indent=2, sort_keys=True),
        "```",
        "</details>",
        "",
        "<details><summary>End of the ctest log</summary>",
        "",
        "````",
    ] + log_tail + ["````", "</details>", ""]
    return "\n".join(lines)


# --- submit ----------------------------------------------------------------

def pr_head(repo, pr):
    return run(["gh", "api", "repos/%s/pulls/%d" % (repo, pr), "--jq", ".head.sha"])


def submit(args):
    with open(os.path.join(args.out_dir, "evidence.json")) as f:
        ev = json.load(f)
    head = pr_head(args.repo, args.pr)
    if ev["head_sha"] != head and not args.force:
        sys.exit("error: the evidence is for %s but %s#%d is now at %s.\n"
                 "Re-run the tests at the new head, or pass --force to post anyway "
                 "(the check will still fail)." % (ev["head_sha"], args.repo, args.pr, head))

    body = os.path.join(args.out_dir, "comment.md")
    print(run(["gh", "pr", "comment", str(args.pr), "--repo", args.repo, "--body-file", body]))

    # Re-run the check that was waiting for this comment. The workflow only
    # runs on pull_request events, and a comment is not one.
    runs = json.loads(run(["gh", "run", "list", "--repo", args.repo,
                           "--workflow", "hpc-evidence.yml", "--commit", head,
                           "--json", "databaseId,conclusion,status", "--limit", "1"]))
    if not runs:
        print("No hpc-evidence run found for %s; it will check the comment on "
              "the next push." % head[:12])
    elif runs[0]["status"] != "completed":
        print("hpc-evidence is still running for %s; re-run it if it does not "
              "pick the comment up." % head[:12])
    elif runs[0]["conclusion"] != "success":
        run(["gh", "run", "rerun", str(runs[0]["databaseId"]), "--repo", args.repo])
        print("Re-running hpc-evidence run %d." % runs[0]["databaseId"])
    return 0


# --- verify ----------------------------------------------------------------

def evidence_comments(repo, pr):
    """(comment, evidence) for each evidence comment on the PR, oldest first."""
    pages = run(["gh", "api", "--paginate", "--slurp",
                 "repos/%s/issues/%d/comments?per_page=100" % (repo, pr)])
    found = []
    for page in json.loads(pages):
        for c in page:
            body = c.get("body") or ""
            if MARKER not in body:
                continue
            m = re.search(r"```json\r?\n(.*?)\r?\n```", body, re.S)
            try:
                ev = json.loads(m.group(1)) if m else None
            except ValueError:
                ev = None
            found.append((c, ev))
    return found


def verify(args):
    with open(BIG_TEST_SOURCE) as f:
        pf_source = f.read()

    report = []
    candidates = []
    for c, ev in evidence_comments(args.repo, args.pr):
        who = "%s (%s)" % (c["user"]["login"], c["author_association"])
        if c["author_association"] not in TRUSTED_ASSOCIATIONS:
            report.append("- ignored %s by %s: no write access" % (c["html_url"], who))
        elif ev is None:
            report.append("- ignored %s by %s: no readable evidence record" % (c["html_url"], who))
        elif ev.get("head_sha") != args.head_sha:
            report.append("- ignored %s by %s: for %s, an older commit"
                          % (c["html_url"], who, str(ev.get("head_sha"))[:12]))
        else:
            candidates.append((c, ev, who))

    if not candidates:
        msg = ("No HPC test evidence for %s. Run scripts/hpc-tests/run_hpc_tests.sh "
               "at that commit and post the result with `hpc_evidence.py submit`." % args.head_sha)
        print("::error::" + msg)
        summary(["## HPC test evidence: missing", "", msg, ""] + report)
        return 1

    # The newest comment for this commit decides, so a failed run can be
    # superseded by a later passing one, but not the other way round.
    c, ev, who = candidates[-1]
    problems = check_evidence(ev, args.head_sha, pf_source)
    head = "## HPC test evidence: %s" % ("FAILED" if problems else "passed")
    lines = [head, "", "From %s by %s, run on `%s`." % (c["html_url"], who, ev.get("host")), ""]
    if problems:
        for p in problems:
            print("::error::" + p)
        lines += ["Problems:", ""] + ["- " + p for p in problems] + [""]
    else:
        lines += ["| Test | Result | Time |", "|---|---|---|"]
        lines += ["| `%s` | %s | %.0f s |" % (n, t["status"], t["time_s"])
                  for n, t in sorted(ev["tests"].items())]
        lines += ["", "All %d big-test reductions are within tolerance of %s."
                  % (len(REDUCTIONS) + len(COUNTS), BIG_TEST_SOURCE), ""]
        print("Evidence %s accepted." % c["html_url"])
    summary(lines + report)
    return 1 if problems else 0


def summary(lines):
    path = os.environ.get("GITHUB_STEP_SUMMARY")
    text = "\n".join(lines) + "\n"
    if path:
        with open(path, "a") as f:
            f.write(text)
    else:
        print(text)


def main():
    p = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    sub = p.add_subparsers(dest="command")
    sub.required = True

    c = sub.add_parser("collect", help="write the evidence for a finished ctest run")
    c.add_argument("build_dir")
    c.add_argument("source_dir")
    c.add_argument("out_dir")
    c.add_argument("--junit", required=True, help="ctest --output-junit report")
    c.add_argument("--log", required=True, help="ctest -V output")
    c.add_argument("--ctest-exit-code", type=int, required=True)
    c.add_argument("--notes", help="free text to include in the comment")
    c.set_defaults(func=collect)

    s = sub.add_parser("submit", help="post the evidence to a pull request")
    s.add_argument("out_dir")
    s.add_argument("--pr", type=int, required=True)
    s.add_argument("--repo", default="UKFELs/Puffin")
    s.add_argument("--force", action="store_true",
                   help="post even though the PR head has moved on")
    s.set_defaults(func=submit)

    v = sub.add_parser("verify", help="check a pull request's evidence (used by CI)")
    v.add_argument("--pr", type=int, required=True)
    v.add_argument("--head-sha", required=True)
    v.add_argument("--repo", default="UKFELs/Puffin")
    v.set_defaults(func=verify)

    args = p.parse_args()
    sys.exit(args.func(args))


if __name__ == "__main__":
    main()
