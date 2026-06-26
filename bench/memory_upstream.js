#!/usr/bin/env node
"use strict";

const datascriptPath = process.env.UPSTREAM_DATASCRIPT_JS;

if (!datascriptPath) {
  console.error("Set UPSTREAM_DATASCRIPT_JS to the upstream DataScript JS bundle.");
  process.exit(2);
}

const d = require(datascriptPath);

const defaultConfig = { size: 1000, txSize: 20 };

function parseArgs(argv) {
  const config = { ...defaultConfig };
  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    const value = argv[i + 1];
    if (arg === "--size") {
      config.size = Number(value);
      i += 1;
    } else if (arg === "--tx-size") {
      config.txSize = Number(value);
      i += 1;
    } else {
      throw new Error(`unknown memory benchmark argument: ${arg}`);
    }
  }
  return config;
}

let blackhole = 0;

function consumeInt(value) {
  blackhole = (blackhole + value) & 0x3fffffff;
}

function report(runtime, scenario) {
  const memory = process.memoryUsage();
  console.log(`${runtime}\t${scenario}\t${memory.rss}\t${memory.heapUsed}`);
}

const schema = {
  id: { ":db/unique": ":db.unique/identity" },
  name: { ":db/index": true },
  age: { ":db/index": true },
  salary: { ":db/index": true },
  status: { ":db/index": true },
  score: { ":db/index": true },
  friend: { ":db/valueType": ":db.type/ref" },
  mentor: { ":db/valueType": ":db.type/ref" },
  team: { ":db/valueType": ":db.type/ref", ":db/cardinality": ":db.cardinality/many" },
  alias: { ":db/cardinality": ":db.cardinality/many" },
};

const names = ["Ivan", "Petr", "Sergey", "Oleg", "Yuri", "Dmitry", "Fedor", "Denis"];
const statuses = ["todo", "doing", "done", "blocked"];
const refAttrs = new Set(["friend", "mentor", "team"]);

function person(size, i) {
  const friend = i === size ? 1 : i + 1;
  const mentor = i <= 10 ? 1 : i - 10;
  return {
    ":db/id": i,
    id: i,
    name: names[(i - 1) % names.length],
    age: (i * 37) % 100,
    salary: (i * 7919) % 100000,
    status: statuses[i % statuses.length],
    score: (i * 13) % 10000,
    friend,
    mentor,
    team: [((i + 7) % size) + 1, ((i + 19) % size) + 1],
    alias: [`alias-${i % 64}`, `tag-${i % 251}`],
  };
}

function people(size) {
  const result = [];
  for (let i = 1; i <= size; i += 1) {
    result.push(person(size, i));
  }
  return result;
}

function buildDb(size) {
  return d.db_with(d.empty_db(schema), people(size));
}

function updateEntity(size, i) {
  const entityId = ((i * 17) % size) + 1;
  return {
    ":db/id": entityId,
    status: statuses[(i + 1) % statuses.length],
    score: (i * 97) % 10000,
    alias: [`updated-${i % 128}`],
  };
}

function runQueries(db) {
  consumeInt(d.datoms(db, ":aevt", "name").length);
  consumeInt(d.q('[:find ?e ?a :where [?e "name" "Ivan"] [?e "age" ?a]]', db).length);
  consumeInt(d.q('[:find ?e ?s :where [?e "salary" ?s] [(> ?s 50000)]]', db).length);
  consumeInt(d.q('[:find ?e ?score :where [?e "status" "doing"] [?e "score" ?score] [(> ?score 500)]]', db).length);
  for (let entityId = 1; entityId <= 100; entityId += 1) {
    const entity = d.pull(db, '["name" "status" {"friend" ["name" "age"]}]', entityId);
    consumeInt(entity ? Object.keys(entity).length : 0);
  }
}

function runScenario(config, db) {
  runQueries(db);
  const txData = [];
  for (let i = 0; i < config.txSize; i += 1) {
    txData.push(updateEntity(config.size, i));
  }
  const nextDb = d.db_with(db, txData);
  runQueries(nextDb);
  return nextDb;
}

function forceGc() {
  if (global.gc) {
    for (let i = 0; i < 4; i += 1) {
      global.gc();
    }
  }
}

function datomField(datom, name, index) {
  if (Array.isArray(datom)) return datom[index];
  if (Object.prototype.hasOwnProperty.call(datom, name)) return datom[name];
  const keywordName = `:${name}`;
  if (Object.prototype.hasOwnProperty.call(datom, keywordName)) return datom[keywordName];
  return datom[name];
}

function attrName(attr) {
  return typeof attr === "string" && attr.startsWith(":") ? attr.slice(1) : String(attr);
}

function canonicalValue(attr, value) {
  if (value === null || value === undefined) return "nil";
  if (typeof value === "number") {
    return refAttrs.has(attr) ? `ref:${value}` : `int:${value}`;
  }
  if (typeof value === "string") {
    return value.startsWith(":") ? `keyword:${value.slice(1)}` : `string:${value}`;
  }
  if (typeof value === "boolean") return `bool:${value}`;
  return `compound:${JSON.stringify(value)}`;
}

function canonicalDatomLine(datom) {
  const entityId = datomField(datom, "e", 0);
  const attr = attrName(datomField(datom, "a", 1));
  const value = datomField(datom, "v", 2);
  return `datom\t${entityId}\t${attr}\t${canonicalValue(attr, value)}`;
}

function writeFinalData(db) {
  const path = process.env.MEMORY_VERIFY_FILE;
  if (!path) return;
  const fs = require("fs");
  const lines = d.datoms(db, ":eavt").map(canonicalDatomLine).sort();
  fs.writeFileSync(path, `${lines.join("\n")}\n`);
}

function main() {
  const config = parseArgs(process.argv.slice(2));
  const runtime = process.env.MEMORY_RUNTIME_LABEL || "upstream-cljs-js";
  let db = buildDb(config.size);
  forceGc();
  report(runtime, "initial-open");
  db = runScenario(config, db);
  forceGc();
  report(runtime, "after-transact-query");
  forceGc();
  report(runtime, "after-gc");
  writeFinalData(db);
  consumeInt(d.datoms(db, ":eavt").length);
  console.error(`blackhole=${blackhole}`);
}

main();
