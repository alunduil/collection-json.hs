#!/usr/bin/env python3
"""Validate CHANGELOG.md against Keep a Changelog 1.1.0 with PVP versions.

Existing validators parse versions as semver and reject four-component PVP
versions such as 1.3.1.3.
"""

import datetime
import re
import sys

CATEGORIES = {"Added", "Changed", "Deprecated", "Removed", "Fixed", "Security"}

UNRELEASED = re.compile(r"## \[(Unreleased)\]")
RELEASE = re.compile(r"## \[(\d+(?:\.\d+)+)\] - (\d{4}-\d{2}-\d{2})")
REFERENCE = re.compile(r"\[([^\]]+)\]: \S+")


def check_sections(sections, labels):
    previous = None
    previous_day = None
    for index, (n, line) in enumerate(sections):
        unreleased = UNRELEASED.fullmatch(line)
        if unreleased:
            if index != 0:
                yield n, "[Unreleased] must be the first section"
            labels[unreleased.group(1)] = n
            continue
        if index == 0:
            yield n, "first section must be ## [Unreleased]"
        release = RELEASE.fullmatch(line)
        if not release:
            yield n, "expected ## [<version>] - YYYY-MM-DD"
            continue
        label, date = release.groups()
        try:
            day = datetime.date.fromisoformat(date)
        except ValueError:
            yield n, f"invalid date {date}"
            day = None
        if day is not None and previous_day is not None and day > previous_day:
            yield n, f"{date} is later than the release above it"
        previous_day = day or previous_day
        version = tuple(int(c) for c in label.split("."))
        if previous is not None and version >= previous:
            yield n, f"{label} is not below the version above it"
        previous = version
        labels[label] = n


def check_categories(lines, sections):
    category = None
    for n, line in lines:
        if line.startswith("#") or REFERENCE.fullmatch(line):
            if category is not None:
                yield category, "empty category"
            category = None
        if line.startswith("### "):
            name = line[len("### ") :]
            if name not in CATEGORIES:
                yield n, f"unknown category {name}"
            if not sections or n < sections[0][0]:
                yield n, "category outside any section"
            category = n
        elif line.strip() and not line.startswith("#"):
            category = None
    if category is not None:
        yield category, "empty category"


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
    labels = {}
    yield from check_sections(sections, labels)
    yield from check_categories(lines, sections)
    yield from check_references(lines, labels)


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
