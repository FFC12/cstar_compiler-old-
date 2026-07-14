"use strict";

const fs = require("fs");
const path = require("path");
const { pathToFileURL } = require("url");
const { analyzeText } = require("./analyzer");

const files = process.argv.slice(2);

if (files.length === 0) {
  console.error("Usage: npm run analyze -- <file.cstar> [more.cstar]");
  process.exitCode = 2;
} else {
  let hasErrors = false;
  for (const file of files) {
    const absolute = path.resolve(file);
    const text = fs.readFileSync(absolute, "utf8");
    const diagnostics = analyzeText(text, pathToFileURL(absolute).toString());
    for (const diagnostic of diagnostics) {
      const line = diagnostic.range.start.line + 1;
      const column = diagnostic.range.start.character + 1;
      const severity = diagnostic.severity === 1 ? "error" : "warning";
      console.log(`${file}:${line}:${column}: ${severity} ${diagnostic.code}: ${diagnostic.message}`);
      if (diagnostic.severity === 1) hasErrors = true;
    }
    if (diagnostics.length === 0) {
      console.log(`${file}: ok`);
    }
  }
  process.exitCode = hasErrors ? 1 : 0;
}
