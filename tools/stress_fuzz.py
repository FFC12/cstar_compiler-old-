#!/usr/bin/env python3
"""Seeded stress/fuzz runner for the C* compiler.

The runner intentionally does not depend on external fuzzing frameworks.  It
generates reproducible C* programs, mutates a small corpus, compiles them, and
records crash/hang reproducers under tests/stress/out/.
"""

from __future__ import annotations

import argparse
import os
import random
import shutil
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
DEFAULT_OUT = ROOT / "tests" / "stress" / "out"


@dataclass
class RunResult:
    path: Path
    kind: str
    returncode: int | None
    elapsed_ms: int
    stdout: str
    stderr: str
    timed_out: bool = False


def find_cstar(build_dir: Path | None) -> Path:
    candidates: list[Path] = []
    if build_dir is not None:
        candidates.extend(
            [
                build_dir / "cstar",
                build_dir / "cstar.exe",
                build_dir / "Debug" / "cstar",
                build_dir / "Debug" / "cstar.exe",
                build_dir / "Release" / "cstar",
                build_dir / "Release" / "cstar.exe",
            ]
        )
    candidates.extend(
        [
            ROOT / "build" / "cstar",
            ROOT / "build" / "cstar.exe",
            ROOT / "build-ucrt" / "cstar",
            ROOT / "build-ucrt" / "cstar.exe",
        ]
    )
    for candidate in candidates:
        if candidate.exists():
            return candidate
    raise SystemExit("cstar executable not found; build the compiler first")


def write_case(path: Path, source: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(source, encoding="utf-8")


def gen_expression(rng: random.Random, names: list[str], depth: int) -> str:
    if depth <= 0 or rng.random() < 0.32:
        choices = names + [str(rng.randint(-64, 64))]
        return rng.choice(choices)

    op = rng.choice(["+", "-", "*", "/", "%", "<", "<=", ">", ">=", "==", "!="])
    left = gen_expression(rng, names, depth - 1)
    right = gen_expression(rng, names, depth - 1)
    if op in ["/", "%"] and rng.random() < 0.55:
        right = str(rng.randint(1, 17))
    return f"({left} {op} {right})"


def gen_valid_program(rng: random.Random, index: int) -> str:
    count = rng.randint(4, 18)
    lines = ["// generated: valid", f"// seed-index: {index}", "", "main() :: int32 {"]
    names: list[str] = []
    for i in range(count):
        name = f"v{i}"
        names.append(name)
        if i == 0:
            expr = str(rng.randint(0, 31))
        else:
            expr = gen_expression(rng, names[:-1], rng.randint(1, 4))
        lines.append(f"  int32 {name} = cast<int32>({expr});")
        if rng.random() < 0.28:
            lines.append(f"  {name} += {rng.randint(-8, 8)};")

    if rng.random() < 0.45:
        arr_len = rng.randint(2, 8)
        values = ", ".join(str(rng.randint(0, 9)) for _ in range(arr_len))
        lines.append(f"  int32 arr[{arr_len}] = ({values});")
        lines.append("  int32 sum = 0;")
        lines.append("  loop(value in arr) {")
        lines.append("    sum += value;")
        lines.append("  }")
        names.append("sum")

    if rng.random() < 0.35:
        lines.append("  if (" + gen_expression(rng, names, 2) + ") {")
        lines.append(f"    {names[-1]} += 1;")
        lines.append("  } else {")
        lines.append(f"    {names[-1]} -= 1;")
        lines.append("  }")

    lines.append(f"  ret cast<int32>({gen_expression(rng, names, 3)});")
    lines.append("}")
    return "\n".join(lines) + "\n"


def gen_valid_struct_program(rng: random.Random, index: int) -> str:
    x = rng.randint(-20, 20)
    y = rng.randint(-20, 20)
    return f"""// generated: valid-struct
// seed-index: {index}

struct Vec {{
  public int32 x;
  public int32 y;

  constructor(int32 x, int32 y) {{
    self.x = x;
    self.y = y;
  }}

  magnitude() :: int32 {{
    int32 ax = self.x;
    int32 ay = self.y;
    if (ax < 0) {{
      ax = 0 - ax;
    }}
    if (ay < 0) {{
      ay = 0 - ay;
    }}
    ret ax + ay;
  }}
}}

main() :: int32 {{
  Vec value = Vec({x}, {y});
  ret value.magnitude();
}}
"""


def gen_invalid_program(rng: random.Random, index: int) -> str:
    variants = [
        """main() :: int32 {
  int32 x = 1;
  int32^ owned = ref x;
  int32^ copy = owned;
  ret deref copy;
}
""",
        """main() :: int32 {
  int32 value = 4;
  const int32** pp = ref value;
  ret 0;
}
""",
        """struct Node {
  Node child;
}
main() :: int32 { ret 0; }
""",
        """main() :: int32 {
  int32 values[2] = (1, 2, 3);
  ret values[0];
}
""",
    ]
    source = rng.choice(variants)
    return f"// generated: invalid\n// seed-index: {index}\n\n{source}"


def mutate_source(rng: random.Random, source: str) -> str:
    tokens = ["\n", " ", "(", ")", "{", "}", ";", ",", "::", "ref", "deref", "ret"]
    data = list(source)
    operations = rng.randint(1, 8)
    for _ in range(operations):
        if not data:
            break
        op = rng.choice(["insert", "delete", "replace"])
        pos = rng.randrange(len(data))
        if op == "insert":
            data[pos:pos] = list(rng.choice(tokens))
        elif op == "delete":
            del data[pos : min(len(data), pos + rng.randint(1, 5))]
        else:
            replacement = rng.choice(tokens)
            data[pos : min(len(data), pos + rng.randint(1, 4))] = list(replacement)
    return "".join(data)


def corpus_files() -> list[Path]:
    roots = [ROOT / "examples" / "smoke", ROOT / "std"]
    files: list[Path] = []
    for root in roots:
        if root.exists():
            files.extend(sorted(root.rglob("*.cstar")))
    return [path for path in files if "modules" not in path.parts]


def run_case(cstar: Path, path: Path, timeout: float) -> RunResult:
    start = time.monotonic()
    try:
        proc = subprocess.run(
            [str(cstar), str(path)],
            cwd=ROOT,
            text=True,
            capture_output=True,
            timeout=timeout,
        )
        elapsed_ms = int((time.monotonic() - start) * 1000)
        return RunResult(path, "compile", proc.returncode, elapsed_ms, proc.stdout, proc.stderr)
    except subprocess.TimeoutExpired as exc:
        elapsed_ms = int((time.monotonic() - start) * 1000)
        return RunResult(
            path,
            "compile",
            None,
            elapsed_ms,
            exc.stdout or "",
            exc.stderr or "",
            timed_out=True,
        )


def is_crash(result: RunResult) -> bool:
    if result.timed_out:
        return True
    if result.returncode is None:
        return True
    if result.returncode >= 128 or result.returncode < 0:
        return True
    combined = result.stdout + "\n" + result.stderr
    crash_needles = ["Assertion failed", "assert", "Segmentation fault", "AddressSanitizer", "Traceback"]
    return any(needle in combined for needle in crash_needles)


def save_failure(out_dir: Path, result: RunResult, label: str) -> None:
    repro_dir = out_dir / "failures"
    repro_dir.mkdir(parents=True, exist_ok=True)
    target = repro_dir / result.path.name
    shutil.copyfile(result.path, target)
    log = target.with_suffix(target.suffix + ".log")
    log.write_text(
        f"label={label}\nreturncode={result.returncode}\n"
        f"elapsed_ms={result.elapsed_ms}\ntimeout={result.timed_out}\n\n"
        f"STDOUT:\n{result.stdout}\n\nSTDERR:\n{result.stderr}\n",
        encoding="utf-8",
    )


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description="Run seeded C* stress/fuzz tests")
    parser.add_argument("--build-dir", type=Path)
    parser.add_argument("--out-dir", type=Path, default=DEFAULT_OUT)
    parser.add_argument("--seed", type=int, default=0xC57A)
    parser.add_argument("--cases", type=int, default=150)
    parser.add_argument("--timeout", type=float, default=5.0)
    parser.add_argument("--mutations", type=int, default=50)
    args = parser.parse_args(argv)

    rng = random.Random(args.seed)
    cstar = find_cstar(args.build_dir)
    out_dir = args.out_dir
    generated_dir = out_dir / "generated"
    if generated_dir.exists():
        shutil.rmtree(generated_dir)
    generated_dir.mkdir(parents=True, exist_ok=True)

    cases: list[Path] = []
    for index in range(args.cases):
        if index % 5 == 0:
            source = gen_invalid_program(rng, index)
        elif index % 3 == 0:
            source = gen_valid_struct_program(rng, index)
        else:
            source = gen_valid_program(rng, index)
        path = generated_dir / f"generated_{index:04d}.cstar"
        write_case(path, source)
        cases.append(path)

    corpus = corpus_files()
    for index in range(args.mutations):
        if not corpus:
            break
        source = rng.choice(corpus).read_text(encoding="utf-8", errors="ignore")
        path = generated_dir / f"mutated_{index:04d}.cstar"
        write_case(path, mutate_source(rng, source))
        cases.append(path)

    crash_count = 0
    diagnostic_count = 0
    pass_count = 0
    slowest: list[RunResult] = []

    start_all = time.monotonic()
    for index, path in enumerate(cases, 1):
        result = run_case(cstar, path, args.timeout)
        slowest.append(result)
        slowest = sorted(slowest, key=lambda item: item.elapsed_ms, reverse=True)[:10]

        if is_crash(result):
            crash_count += 1
            save_failure(out_dir, result, "crash-or-timeout")
            print(f"[CRASH] {path.name} rc={result.returncode} elapsed={result.elapsed_ms}ms")
        elif result.returncode == 0:
            pass_count += 1
        else:
            diagnostic_count += 1

        if index % 25 == 0:
            print(
                f"[PROGRESS] {index}/{len(cases)} pass={pass_count} "
                f"diag={diagnostic_count} crash={crash_count}"
            )

    elapsed = time.monotonic() - start_all
    summary = out_dir / "summary.txt"
    summary.write_text(
        "\n".join(
            [
                f"seed={args.seed}",
                f"cases={len(cases)}",
                f"pass={pass_count}",
                f"diagnostic={diagnostic_count}",
                f"crash_or_timeout={crash_count}",
                f"elapsed_seconds={elapsed:.3f}",
                "slowest=",
                *[
                    f"  {item.elapsed_ms}ms {item.path.name} rc={item.returncode}"
                    for item in slowest
                ],
            ]
        )
        + "\n",
        encoding="utf-8",
    )

    print(summary.read_text(encoding="utf-8"), end="")
    return 1 if crash_count else 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
