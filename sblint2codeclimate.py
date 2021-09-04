#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import json


severity_map = {"error": "blocker", "warning": "major"}


def severity(category_name):
    return next(iter([v for k, v in severity_map.items()
                      if k in category_name]), "info")


warnings = []
for line in sys.stdin:
    try:
        location, category, *text = line.split(" ")
        filename, line, column, _ = location.split(":")

        category = category[:-1]
        text = " ".join(text)[:-1]
        line = int(line)
        column = int(column)

        warning = {
            "engine_name": "sblint",
            "type": "issue",
            "check_name": "sblint",
            "categories": ["Style"],
            "description": text,
            "severity": severity(category),
            "location": {
                "path": filename,
                "positions": {
                    "begin": {"line": line, "column": column},
                    "end": {"line": line, "column": column + 1},
                },
            },
        }
        warnings.append(warning)
    except:
        continue

with open("gl-code-quality-report.json", "w") as f:
    f.write(json.dumps(warnings))
