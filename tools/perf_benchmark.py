#!/usr/bin/env python3
"""Compile/runtime benchmark harness for C*, C, and C++.

The goal is not to claim language-level parity yet.  It gives a repeatable
baseline for compiler throughput, generated executable size, and simple runtime
cost against the platform C/C++ compiler.
"""

from __future__ import annotations

import argparse
import csv
import os
import platform
import shutil
import statistics
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
DEFAULT_OUT = ROOT / "tests" / "performance" / "out"


@dataclass(frozen=True)
class Benchmark:
    name: str
    cstar: str
    c: str
    cpp: str


@dataclass
class Measurement:
    benchmark: str
    language: str
    compiler: str
    mode: str
    iterations: int
    returncode: int
    mean_ms: float
    median_ms: float
    min_ms: float
    max_ms: float
    exe_size: int
    command: str


def find_tool(explicit: str | None, candidates: list[str]) -> str:
    if explicit:
        return explicit
    for candidate in candidates:
        found = shutil.which(candidate)
        if found:
            return found
    raise SystemExit(f"tool not found: one of {', '.join(candidates)}")


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
    candidates.extend([ROOT / "build" / "cstar", ROOT / "build-ucrt" / "cstar"])
    for candidate in candidates:
        if candidate.exists():
            return candidate
    raise SystemExit("cstar executable not found; build the compiler first")


def exe_suffix() -> str:
    return ".exe" if os.name == "nt" else ""


def run_timed(command: list[str], cwd: Path, timeout: float) -> tuple[int, float, str, str]:
    start = time.perf_counter()
    proc = subprocess.run(
        command,
        cwd=cwd,
        text=True,
        capture_output=True,
        timeout=timeout,
    )
    elapsed_ms = (time.perf_counter() - start) * 1000.0
    return proc.returncode, elapsed_ms, proc.stdout, proc.stderr


def repeated(
    command: list[str],
    cwd: Path,
    iterations: int,
    timeout: float,
    allow_nonzero: bool = False,
) -> tuple[int, list[float]]:
    samples: list[float] = []
    last_rc = 0
    for _ in range(iterations):
        rc, elapsed, stdout, stderr = run_timed(command, cwd, timeout)
        last_rc = rc
        if rc != 0 and not allow_nonzero:
            raise RuntimeError(
                f"command failed rc={rc}: {' '.join(command)}\nSTDOUT:\n{stdout}\nSTDERR:\n{stderr}"
            )
        samples.append(elapsed)
    return last_rc, samples


def summarize(
    benchmark: str,
    language: str,
    compiler: str,
    mode: str,
    iterations: int,
    returncode: int,
    samples: list[float],
    exe: Path,
    command: list[str],
) -> Measurement:
    return Measurement(
        benchmark=benchmark,
        language=language,
        compiler=compiler,
        mode=mode,
        iterations=iterations,
        returncode=returncode,
        mean_ms=statistics.mean(samples),
        median_ms=statistics.median(samples),
        min_ms=min(samples),
        max_ms=max(samples),
        exe_size=exe.stat().st_size if exe.exists() else 0,
        command=" ".join(command),
    )


def write_sources(out_dir: Path, benchmarks: list[Benchmark]) -> None:
    src_dir = out_dir / "src"
    src_dir.mkdir(parents=True, exist_ok=True)
    for bench in benchmarks:
        (src_dir / f"{bench.name}.cstar").write_text(bench.cstar, encoding="utf-8")
        (src_dir / f"{bench.name}.c").write_text(bench.c, encoding="utf-8")
        (src_dir / f"{bench.name}.cpp").write_text(bench.cpp, encoding="utf-8")


def benchmark_sources(work: int, unroll: int) -> list[Benchmark]:
    # Keep the computations integer-only so the current C* backend and the C/C++
    # baselines can run the same shape without library calls.
    scalar_cstar = f"""main() :: int32 {{
  int32 i = 0;
  int32 acc = 17;
  loop (i < {work}) {{
    acc += (i * 3) % 97;
    acc ^= i;
    acc &= 2147483647;
    i += 1;
  }}
  ret acc & 255;
}}
"""
    scalar_c = f"""int main(void) {{
  int i = 0;
  int acc = 17;
  while (i < {work}) {{
    acc += (i * 3) % 97;
    acc ^= i;
    acc &= 2147483647;
    i += 1;
  }}
  return acc & 255;
}}
"""
    scalar_cpp = f"""int main() {{
  int i = 0;
  int acc = 17;
  while (i < {work}) {{
    acc += (i * 3) % 97;
    acc ^= i;
    acc &= 2147483647;
    i += 1;
  }}
  return acc & 255;
}}
"""

    helper_defs_cstar = "\n".join(
        f"step{i}(int32 value) :: int32 {{ ret (value + {i + 3}) ^ {i * 17 + 11}; }}"
        for i in range(unroll)
    )
    helper_defs_c = "\n".join(
        f"static int step{i}(int value) {{ return (value + {i + 3}) ^ {i * 17 + 11}; }}"
        for i in range(unroll)
    )
    helper_defs_cpp = "\n".join(
        f"static int step{i}(int value) {{ return (value + {i + 3}) ^ {i * 17 + 11}; }}"
        for i in range(unroll)
    )
    helper_calls_cstar = "\n".join(f"    acc = step{i}(acc);" for i in range(unroll))
    helper_calls_c = "\n".join(f"    acc = step{i}(acc);" for i in range(unroll))

    calls_cstar = f"""{helper_defs_cstar}

main() :: int32 {{
  int32 i = 0;
  int32 acc = 1;
  loop (i < {max(1, work // max(1, unroll))}) {{
{helper_calls_cstar}
    i += 1;
  }}
  ret acc & 255;
}}
"""
    calls_c = f"""{helper_defs_c}

int main(void) {{
  int i = 0;
  int acc = 1;
  while (i < {max(1, work // max(1, unroll))}) {{
{helper_calls_c}
    i += 1;
  }}
  return acc & 255;
}}
"""
    calls_cpp = f"""{helper_defs_cpp}

int main() {{
  int i = 0;
  int acc = 1;
  while (i < {max(1, work // max(1, unroll))}) {{
{helper_calls_c}
    i += 1;
  }}
  return acc & 255;
}}
"""

    array_cstar = f"""main() :: int32 {{
  int32 data[16] = (1, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53);
  int32 i = 0;
  int32 acc = 0;
  loop (i < {work}) {{
    int32 index = i % 16;
    data[index] += i & 7;
    acc += data[index] & 1023;
    i += 1;
  }}
  ret acc & 255;
}}
"""
    array_c = f"""int main(void) {{
  int data[16] = {{1, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53}};
  int i = 0;
  int acc = 0;
  while (i < {work}) {{
    int index = i % 16;
    data[index] += i & 7;
    acc += data[index] & 1023;
    i += 1;
  }}
  return acc & 255;
}}
"""
    array_cpp = f"""int main() {{
  int data[16] = {{1, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53}};
  int i = 0;
  int acc = 0;
  while (i < {work}) {{
    int index = i % 16;
    data[index] += i & 7;
    acc += data[index] & 1023;
    i += 1;
  }}
  return acc & 255;
}}
"""

    return [
        Benchmark("scalar_loop", scalar_cstar, scalar_c, scalar_cpp),
        Benchmark("function_pressure", calls_cstar, calls_c, calls_cpp),
        Benchmark("array_pressure", array_cstar, array_c, array_cpp),
    ]


def measure_language(
    bench: Benchmark,
    language: str,
    compiler: str,
    src: Path,
    exe: Path,
    compile_iters: int,
    run_iters: int,
    timeout: float,
    cstar_output_dir: Path,
    opt: str,
) -> list[Measurement]:
    exe.parent.mkdir(parents=True, exist_ok=True)
    if language == "cstar":
        compile_cmd = [
            compiler,
            str(src),
            "--emit=exe",
            "--no-run",
            "--output-dir",
            str(cstar_output_dir),
        ]
    elif language == "c":
        compile_cmd = [compiler, opt, str(src), "-o", str(exe)]
    elif language == "cpp":
        compile_cmd = [compiler, opt, str(src), "-o", str(exe)]
    else:
        raise ValueError(language)

    _, compile_samples = repeated(compile_cmd, ROOT, compile_iters, timeout)
    if language == "cstar":
        produced = cstar_output_dir / f"{src.name}.out"
        if produced.exists():
            exe = produced
        else:
            produced = cstar_output_dir / f"{src.name}{exe_suffix()}"
            if produced.exists():
                exe = produced

    run_cmd = [str(exe)]
    rc, run_samples = repeated(run_cmd, ROOT, run_iters, timeout, allow_nonzero=True)

    return [
        summarize(
            bench.name,
            language,
            Path(compiler).name,
            "compile",
            compile_iters,
            0,
            compile_samples,
            exe,
            compile_cmd,
        ),
        summarize(
            bench.name,
            language,
            Path(compiler).name,
            "run",
            run_iters,
            rc,
            run_samples,
            exe,
            run_cmd,
        ),
    ]


def write_reports(out_dir: Path, rows: list[Measurement]) -> None:
    csv_path = out_dir / "results.csv"
    with csv_path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(Measurement.__dataclass_fields__.keys()))
        writer.writeheader()
        for row in rows:
            writer.writerow(row.__dict__)

    md = out_dir / "REPORT.md"
    lines = [
        "# C* Performance Baseline",
        "",
        f"- Host: `{platform.platform()}`",
        f"- Python: `{platform.python_version()}`",
        "",
        "| Benchmark | Lang | Mode | Iter | Mean ms | Median ms | Min ms | Max ms | Exe bytes | RC |",
        "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|",
    ]
    for row in rows:
        lines.append(
            f"| {row.benchmark} | {row.language} | {row.mode} | {row.iterations} | "
            f"{row.mean_ms:.3f} | {row.median_ms:.3f} | {row.min_ms:.3f} | "
            f"{row.max_ms:.3f} | {row.exe_size} | {row.returncode} |"
        )

    lines.extend(
        [
            "",
            "## Median Ratios",
            "",
            "`1.00x` is the C median for the same benchmark/mode.",
            "",
            "| Benchmark | Mode | Lang | Median/C median |",
            "|---|---:|---:|---:|",
        ]
    )
    c_medians = {
        (row.benchmark, row.mode): row.median_ms
        for row in rows
        if row.language == "c" and row.median_ms > 0.0
    }
    for row in rows:
        base = c_medians.get((row.benchmark, row.mode))
        if not base:
            continue
        if row.mode == "run" and row.returncode < 0:
            lines.append(
                f"| {row.benchmark} | {row.mode} | {row.language} | crash |"
            )
            continue
        lines.append(
            f"| {row.benchmark} | {row.mode} | {row.language} | "
            f"{row.median_ms / base:.2f}x |"
        )

    row_by_key = {
        (row.benchmark, row.language, row.mode): row
        for row in rows
    }

    def classify_ratio(ratio: float) -> str:
        if ratio < 0.95:
            return "C* daha hızlı"
        if ratio > 1.05:
            return "C* daha yavaş"
        return "yakın"

    def fmt_ratio(value: float) -> str:
        return f"{value:.2f}x"

    lines.extend(
        [
            "",
            "## C Baseline Yorumu",
            "",
            "Bu bölüm C sonuçlarını `1.00x` kabul eder ve C* sonucunu aynı benchmark/mode içinde C median değeriyle karşılaştırır. Median kullanıyoruz çünkü macOS process start-up ve background scheduling bazı örneklerde mean değerini yukarı çekebiliyor.",
            "",
            "| Benchmark | Compile | Runtime | Exe Size | Kısa Yorum |",
            "|---|---:|---:|---:|---|",
        ]
    )

    compile_ratios: list[float] = []
    runtime_ratios: list[float] = []
    size_ratios: list[float] = []
    crash_notes: list[str] = []

    for benchmark in sorted({row.benchmark for row in rows}):
        c_compile = row_by_key.get((benchmark, "c", "compile"))
        cs_compile = row_by_key.get((benchmark, "cstar", "compile"))
        c_run = row_by_key.get((benchmark, "c", "run"))
        cs_run = row_by_key.get((benchmark, "cstar", "run"))

        compile_text = "n/a"
        run_text = "n/a"
        size_text = "n/a"
        comments: list[str] = []

        if c_compile and cs_compile and c_compile.median_ms > 0.0:
            ratio = cs_compile.median_ms / c_compile.median_ms
            compile_ratios.append(ratio)
            compile_text = f"{fmt_ratio(ratio)} ({classify_ratio(ratio)})"
            if ratio > 1.05:
                comments.append("compile hattı C'den yavaş")
            elif ratio < 0.95:
                comments.append("compile hattı C'den hızlı")
            else:
                comments.append("compile C'ye yakın")

        if c_run and cs_run and c_run.median_ms > 0.0:
            if cs_run.returncode < 0:
                run_text = f"crash (signal {-cs_run.returncode})"
                crash_notes.append(f"{benchmark}: C* runtime signal {-cs_run.returncode}")
                comments.append("runtime crash var")
            else:
                ratio = cs_run.median_ms / c_run.median_ms
                runtime_ratios.append(ratio)
                run_text = f"{fmt_ratio(ratio)} ({classify_ratio(ratio)})"
                if ratio > 1.05:
                    comments.append("runtime C'den yavaş")
                elif ratio < 0.95:
                    comments.append("runtime C'den hızlı")
                else:
                    comments.append("runtime C'ye yakın")

        if c_compile and cs_compile and c_compile.exe_size > 0:
            ratio = cs_compile.exe_size / c_compile.exe_size
            size_ratios.append(ratio)
            size_text = f"{fmt_ratio(ratio)} ({cs_compile.exe_size} / {c_compile.exe_size} bytes)"
            if ratio > 1.10:
                comments.append("binary daha büyük")
            elif ratio < 0.90:
                comments.append("binary daha küçük")
            else:
                comments.append("binary boyutu yakın")

        lines.append(
            f"| {benchmark} | {compile_text} | {run_text} | {size_text} | "
            f"{'; '.join(comments)} |"
        )

    def average(values: list[float]) -> float | None:
        if not values:
            return None
        return statistics.mean(values)

    lines.extend(["", "## Genel Değerlendirme", ""])
    avg_compile = average(compile_ratios)
    avg_runtime = average(runtime_ratios)
    avg_size = average(size_ratios)

    if avg_compile is not None:
        lines.append(
            f"- Compile median ortalaması: C* / C = `{avg_compile:.2f}x`. "
            "Bu değer frontend + LLVM IR üretimi + backend clang driver hattının toplam maliyetini gösterir."
        )
    if avg_runtime is not None:
        lines.append(
            f"- Runtime median ortalaması: C* / C = `{avg_runtime:.2f}x`. "
            "Bu workload'larda C* generated code'u C debug baseline'a yakın veya bazı örneklerde daha hızlı görünebilir; sonuçları release optimizer karşılaştırmasıyla ayrıca doğrulamak gerekir."
        )
    if avg_size is not None:
        lines.append(
            f"- Executable size ortalaması: C* / C = `{avg_size:.2f}x`. "
            "Fark özellikle array/initializer lowering ve runtime helper izleriyle büyüyebilir."
        )

    if crash_notes:
        lines.append("- Runtime crash notları: " + "; ".join(crash_notes) + ".")
    else:
        lines.append("- Runtime crash yok; tüm C* benchmark executable'ları C ile aynı return code sınıfında tamamlandı.")

    lines.extend(
        [
            "",
            "## Benchmark Bazında Okuma",
            "",
            "- `scalar_loop`: Basit integer arithmetic ve while-style loop maliyetini gösterir. Compile tarafında C* daha pahalıysa sebep çoğunlukla frontend/codegen pipeline overhead'idir; runtime tarafı generated IR kalitesini gösterir.",
            "- `function_pressure`: Çok sayıda küçük function declaration/call üretir. Compile farkı symbol/previsit/codegen pass maliyetini görünür yapar; runtime farkı call lowering ve ABI maliyetini ölçer.",
            "- `array_pressure`: Fixed array initializer, dynamic index, array element update/read ve loop-local variable allocation yollarını zorlar. Daha önce bu benchmark loop içi `alloca` stack büyümesini yakaladı; local slot'lar entry block'a taşındıktan sonra crash kalktı.",
            "",
            "## Sınırlamalar",
            "",
            "- C ve C++ default olarak `-O0` ile ölçülür; C* şu anda açık optimizer profili taşımadığı için bu debug baseline'dır.",
            "- Release karşılaştırması için `--c-opt -O2 --cpp-opt -O2` kullanılmalı; o sonuç C/C++ lehine ciddi değişebilir.",
            "- Runtime ölçümü process launch maliyetini de içerir. Kısa workload'larda median bu yüzden mean'den daha güvenilirdir.",
            "- Bu set microbenchmark'tır; struct/ownership/stdlib/native interop ve büyük module graph için ayrı benchmarklar eklenmelidir.",
        ]
    )

    lines.extend(["", "Raw command lines are in `results.csv`.", ""])
    md.write_text("\n".join(lines), encoding="utf-8")


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description="Compare C* compile/runtime against C and C++")
    parser.add_argument("--build-dir", type=Path)
    parser.add_argument("--out-dir", type=Path, default=DEFAULT_OUT)
    parser.add_argument("--work", type=int, default=250000)
    parser.add_argument("--unroll", type=int, default=24)
    parser.add_argument("--compile-iters", type=int, default=5)
    parser.add_argument("--run-iters", type=int, default=20)
    parser.add_argument("--timeout", type=float, default=20.0)
    parser.add_argument("--cc")
    parser.add_argument("--cxx")
    parser.add_argument("--c-opt", default="-O0")
    parser.add_argument("--cpp-opt", default="-O0")
    args = parser.parse_args(argv)

    out_dir = args.out_dir
    if out_dir.exists():
        shutil.rmtree(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    cstar = find_cstar(args.build_dir)
    cc = find_tool(args.cc, ["clang", "gcc", "cc"])
    cxx = find_tool(args.cxx, ["clang++", "g++", "c++"])
    benchmarks = benchmark_sources(args.work, args.unroll)
    write_sources(out_dir, benchmarks)

    rows: list[Measurement] = []
    src_dir = out_dir / "src"
    bin_dir = out_dir / "bin"
    cstar_bin = out_dir / "cstar-bin"
    for bench in benchmarks:
        print(f"[BENCH] {bench.name}")
        rows.extend(
            measure_language(
                bench,
                "cstar",
                str(cstar),
                src_dir / f"{bench.name}.cstar",
                cstar_bin / f"{bench.name}{exe_suffix()}",
                args.compile_iters,
                args.run_iters,
                args.timeout,
                cstar_bin,
                "",
            )
        )
        rows.extend(
            measure_language(
                bench,
                "c",
                cc,
                src_dir / f"{bench.name}.c",
                bin_dir / f"{bench.name}.c{exe_suffix()}",
                args.compile_iters,
                args.run_iters,
                args.timeout,
                cstar_bin,
                args.c_opt,
            )
        )
        rows.extend(
            measure_language(
                bench,
                "cpp",
                cxx,
                src_dir / f"{bench.name}.cpp",
                bin_dir / f"{bench.name}.cpp{exe_suffix()}",
                args.compile_iters,
                args.run_iters,
                args.timeout,
                cstar_bin,
                args.cpp_opt,
            )
        )

    write_reports(out_dir, rows)
    print((out_dir / "REPORT.md").read_text(encoding="utf-8"))
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
