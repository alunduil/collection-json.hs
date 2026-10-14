#!/usr/bin/env python3
"""Validate CHANGELOG.md against Keep a Changelog 1.1.0 with PVP versions."""

import datetime
import itertools
import re
import sys
from typing import NamedTuple

CATEGORIES = {"Added", "Changed", "Deprecated", "Removed", "Fixed", "Security"}

UNRELEASED = "Unreleased"
UNRELEASED_HEADING = f"## [{UNRELEASED}]"
RELEASE = re.compile(r"## \[(\d+(?:\.\d+)+)\] - (\d{4}-\d{2}-\d{2})")
REFERENCE = re.compile(r"\[([^\]]+)\]: \S+")


class Release(NamedTuple):
    line: int
    label: str
    date: str

    @property
    def version(self):
        return tuple(int(c) for c in self.label.split("."))

    @property
    def day(self):
        try:
            return datetime.date.fromisoformat(self.date)
        except ValueError:
            return None


def parse_releases(sections):
    return [
        Release(n, *release.groups())
        for n, line in sections
        if (release := RELEASE.fullmatch(line))
    ]


def labels(sections):
    unreleased = {UNRELEASED: n for n, line in sections if line == UNRELEASED_HEADING}
    return unreleased | {r.label: r.line for r in parse_releases(sections)}


def check_headings(sections):
    for index, (n, line) in enumerate(sections):
        if line == UNRELEASED_HEADING:
            if index != 0:
                yield n, f"[{UNRELEASED}] must be the first section"
            continue
        if index == 0:
            yield n, f"first section must be {UNRELEASED_HEADING}"
        if not RELEASE.fullmatch(line):
            yield n, "expected ## [<version>] - YYYY-MM-DD"


def check_dates(releases):
    for r in releases:
        if r.day is None:
            yield r.line, f"invalid date {r.date}"
    dated = [r for r in releases if r.day is not None]
    for above, below in itertools.pairwise(dated):
        if below.day > above.day:
            yield below.line, f"{below.date} is later than the release above it"


def check_versions(releases):
    for above, below in itertools.pairwise(releases):
        if below.version >= above.version:
            yield below.line, f"{below.label} is not below the version above it"


def is_entry(line):
    return not (line.startswith("#") or REFERENCE.fullmatch(line))


def check_categories(lines, sections):
    for index, (n, line) in enumerate(lines):
        if not line.startswith("### "):
            continue
        name = line.removeprefix("### ")
        if name not in CATEGORIES:
            yield n, f"unknown category {name}"
        if not sections or n < sections[0][0]:
            yield n, "category outside any section"
        following = next((text for _, text in lines[index + 1 :] if text.strip()), "")
        if not (following and is_entry(following)):
            yield n, "empty category"


def check_references(lines, labels):
    references = {}
    for n, line in lines:
        reference = REFERENCE.fullmatch(line)
        if reference:
            label = reference.group(1)
            if label in references:
                yield n, f"duplicate reference [{label}]"
            references[label] = n
    for label, n in labels.items():
        if label not in references:
            yield n, f"missing reference [{label}]"
    for label, n in references.items():
        if label not in labels:
            yield n, f"orphan reference [{label}]"


def check(lines):
    sections = [(n, line) for n, line in lines if line.startswith("## ")]
    yield from check_headings(sections)
    yield from check_dates(parse_releases(sections))
    yield from check_versions(parse_releases(sections))
    yield from check_categories(lines, sections)
    yield from check_references(lines, labels(sections))


def read(path):
    with open(path, encoding="utf-8") as f:
        return list(enumerate(f.read().splitlines(), start=1))


def main(paths):
    errors = [
        f"{path}:{n}: {message}"
        for path in paths
        for n, message in sorted(check(read(path)))
    ]
    for e in errors:
        print(e, file=sys.stderr)
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
