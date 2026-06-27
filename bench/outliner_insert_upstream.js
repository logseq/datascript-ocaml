#!/usr/bin/env node
"use strict";

const datascriptPath = process.env.UPSTREAM_DATASCRIPT_JS;

if (!datascriptPath) {
  console.error("Set UPSTREAM_DATASCRIPT_JS to the upstream DataScript JS bundle.");
  process.exit(2);
}

const d = require(datascriptPath);

const defaultConfig = { size: 5000, warmupMs: 300, sampleMs: 700, samples: 7 };

function parseArgs(argv) {
  const config = { ...defaultConfig };
  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    const value = argv[i + 1];
    if (arg === "--size") {
      config.size = Number(value);
      i += 1;
    } else if (arg === "--warmup-ms") {
      config.warmupMs = Number(value);
      i += 1;
    } else if (arg === "--sample-ms") {
      config.sampleMs = Number(value);
      i += 1;
    } else if (arg === "--samples") {
      config.samples = Number(value);
      i += 1;
    } else {
      throw new Error(`unknown outliner insert benchmark argument: ${arg}`);
    }
  }
  return config;
}

function nowMs() {
  const [seconds, nanos] = process.hrtime();
  return seconds * 1000 + nanos / 1e6;
}

function median(values) {
  return values.slice().sort((a, b) => a - b)[Math.floor(values.length / 2)];
}

function formatMs(value) {
  return value > 1 ? value.toFixed(2) : value.toFixed(5);
}

let blackhole = 0;

function consumeInt(value) {
  blackhole = (blackhole + value) & 0x3fffffff;
}

const schema = {
  "block/id": { ":db/unique": ":db.unique/identity" },
  "block/journal-day": { ":db/index": true },
  "block/content": { ":db/index": true },
  "block/order": { ":db/index": true },
  "block/collapsed": { ":db/index": true },
  "block/parent": { ":db/valueType": ":db.type/ref" },
};

function blockTx(index) {
  const id = `block-${String(index).padStart(5, "0")}`;
  return [
    [":db/add", id, "block/id", id],
    [":db/add", id, "block/journal-day", "2026-06-27"],
    [":db/add", id, "block/content", `Block ${index}`],
    [":db/add", id, "block/order", index],
    [":db/add", id, "block/collapsed", false],
  ];
}

function buildDb(size) {
  const tx = [];
  for (let i = 1; i <= size; i += 1) {
    tx.push(...blockTx(i));
  }
  return d.db_with(d.empty_db(schema), tx);
}

function makeInsertTxs(size, count) {
  const txs = [];
  for (let i = 0; i < count; i += 1) {
    const id = `block-new-${String(i).padStart(5, "0")}`;
    const blockIndex = size + i + 1;
    txs.push([
      id,
      [
        [":db/add", id, "block/id", id],
        [":db/add", id, "block/journal-day", "2026-06-27"],
        [":db/add", id, "block/content", `Block ${blockIndex}`],
        [":db/add", id, "block/order", blockIndex],
        [":db/add", id, "block/collapsed", false],
        [":db/add", id, "block/parent", ["block/id", "block-00001"]],
      ],
    ]);
  }
  return txs;
}

function runFor(durationMs, fn) {
  const start = nowMs();
  const deadline = start + durationMs;
  let iterations = 0;
  let elapsed = 0;
  do {
    fn(iterations);
    iterations += 1;
    elapsed = nowMs() - start;
  } while (nowMs() < deadline);
  return { iterations, elapsed };
}

function bench(config, name, fn) {
  runFor(config.warmupMs, fn);
  const samples = [];
  for (let i = 0; i < config.samples; i += 1) {
    const { iterations, elapsed } = runFor(config.sampleMs, fn);
    samples.push(elapsed / iterations);
  }
  console.log(`${name}\t${formatMs(median(samples))}`);
}

function main() {
  const config = parseArgs(process.argv.slice(2));
  console.log("runtime\tupstream-cljs-js");
  console.log(`size\t${config.size}`);
  const db = buildDb(config.size);
  const txs = makeInsertTxs(config.size, 4096);
  bench(config, "insert-one-block", (iteration) => {
    const [id, tx] = txs[iteration & (txs.length - 1)];
    const nextDb = d.db_with(db, tx);
    consumeInt(d.datoms(nextDb, ":avet", "block/id", id).length);
  });
  console.error(`blackhole=${blackhole}`);
}

main();
