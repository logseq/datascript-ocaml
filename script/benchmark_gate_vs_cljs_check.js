#!/usr/bin/env node
"use strict";

const fs = require("fs");

function parseResults(output) {
  const results = {};
  let runtime = null;

  for (const line of output.split(/\r?\n/)) {
    const parts = line.trim().split(/\s+/);
    if (parts.length !== 2) continue;
    if (parts[0] === "runtime") {
      runtime = parts[1];
      results[runtime] = results[runtime] || {};
    } else if (runtime && parts[0] !== "size") {
      results[runtime][parts[0]] = Number(parts[1]);
    }
  }

  return results;
}

function comparableNames(upstream) {
  return Object.keys(upstream).filter((name) => Number.isFinite(upstream[name]));
}

function compareRuntime(results, runtimeName, upstream, names) {
  const failures = [];
  const runtimeResults = results[runtimeName];
  if (!runtimeResults) {
    return [`missing ${runtimeName} benchmark results`];
  }

  for (const name of names) {
    const actual = runtimeResults[name];
    const target = upstream[name];
    if (!Number.isFinite(actual)) {
      failures.push(`missing ${runtimeName} ${name}`);
    } else if (!(actual < target)) {
      failures.push(`${runtimeName} ${name} ${actual}ms is not faster than upstream-cljs-js ${target}ms`);
    }
  }

  return failures;
}

function check(output) {
  const results = parseResults(output);
  const upstream = results["upstream-cljs-js"];
  if (!upstream) {
    return ["missing upstream-cljs-js benchmark results"];
  }

  const names = comparableNames(upstream);
  const failures = [];
  for (const runtimeName of ["ocaml-native", "js_of_ocaml"]) {
    failures.push(...compareRuntime(results, runtimeName, upstream, names));
  }

  return failures;
}

if (require.main === module) {
  const output = fs.readFileSync(0, "utf8");
  const failures = check(output);
  if (failures.length > 0) {
    console.error(failures.join("\n"));
    process.exit(1);
  }
}

module.exports = { check, parseResults };
