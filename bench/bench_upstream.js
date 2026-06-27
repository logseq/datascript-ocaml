#!/usr/bin/env node
"use strict";

const datascriptPath = process.env.UPSTREAM_DATASCRIPT_JS;

if (!datascriptPath) {
  console.error("Set UPSTREAM_DATASCRIPT_JS to the upstream DataScript JS bundle.");
  process.exit(2);
}

const d = require(datascriptPath);

const defaultConfig = { size: 200, warmupMs: 200, sampleMs: 500, samples: 5 };

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
      throw new Error(`unknown benchmark argument: ${arg}`);
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

function consumeDb(db) {
  consumeInt(d.datoms(db, ":eavt").length);
}

function consumeRows(rows) {
  consumeInt(rows.length);
}

function consumePull(value) {
  consumeInt(value ? Object.keys(value).length : 0);
}

function runFor(durationMs, fn) {
  const start = nowMs();
  const deadline = start + durationMs;
  let iterations = 0;
  let elapsed = 0;
  do {
    fn();
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

const schema = {
  id: { ":db/unique": ":db.unique/identity" },
  name: { ":db/index": true },
  age: { ":db/index": true },
  salary: { ":db/index": true },
  friend: { ":db/valueType": ":db.type/ref" },
  alias: { ":db/cardinality": ":db.cardinality/many" },
};

const names = ["Ivan", "Petr", "Sergey", "Oleg", "Yuri", "Dmitry", "Fedor", "Denis"];
const lastNames = ["Ivanov", "Petrov", "Sidorov", "Kovalev", "Kuznetsov", "Voronoi"];

function person(size, i) {
  const friend = i === size ? 1 : i + 1;
  return {
    ":db/id": i,
    id: i,
    name: names[(i - 1) % names.length],
    "last-name": lastNames[(i - 1) % lastNames.length],
    age: (i * 37) % 100,
    salary: (i * 7919) % 100000,
    sex: i % 2 === 0 ? ":male" : ":female",
    friend,
    alias: [`alias-${i % 10}`, `tag-${i % 17}`],
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

function addOneByOne(size) {
  let db = d.empty_db(schema);
  for (const entity of people(size)) {
    db = d.db_with(db, [entity]);
  }
  return db;
}

function addOneDatomPerTx(size) {
  let db = d.empty_db(schema);
  for (const entity of people(size)) {
    const id = entity[":db/id"];
    for (const [attr, value] of Object.entries(entity)) {
      if (attr === ":db/id") continue;
      const values = Array.isArray(value) ? value : [value];
      for (const item of values) {
        db = d.db_with(db, [[":db/add", id, attr, item]]);
      }
    }
  }
  return db;
}

function main() {
  const config = parseArgs(process.argv.slice(2));
  console.log("runtime\tupstream-cljs-js");
  console.log(`size\t${config.size}`);
  let cachedDb = null;
  const db = () => {
    if (cachedDb === null) cachedDb = buildDb(config.size);
    return cachedDb;
  };

  bench(config, "add-1", () => consumeDb(addOneDatomPerTx(config.size)));
  bench(config, "add-5", () => consumeDb(addOneByOne(config.size)));
  bench(config, "add-all", () => consumeDb(buildDb(config.size)));
  bench(config, "datoms-name", () => consumeInt(d.datoms(db(), ":aevt", "name").length));
  bench(config, "q1", () =>
    consumeRows(d.q('[:find ?e :where [?e "name" "Ivan"]]', db()))
  );
  bench(config, "q2", () =>
    consumeRows(d.q('[:find ?e ?a :where [?e "name" "Ivan"] [?e "age" ?a]]', db()))
  );
  bench(config, "q3", () =>
    consumeRows(d.q('[:find ?e ?a :where [?e "name" "Ivan"] [?e "age" ?a] [?e "sex" :male]]', db()))
  );
  bench(config, "q4", () =>
    consumeRows(d.q('[:find ?e ?l ?a :where [?e "name" "Ivan"] [?e "last-name" ?l] [?e "age" ?a] [?e "sex" :male]]', db()))
  );
  bench(config, "q5-shortcircuit", () =>
    consumeRows(d.q('[:find ?e ?n ?l ?a ?s ?al :in $ ?n ?a :where [?e "name" ?n] [?e "age" ?a] [?e "last-name" ?l] [?e "sex" ?s] [?e "alias" ?al]]', db(), "Anastasia", 35))
  );
  bench(config, "qpred1", () =>
    consumeRows(d.q('[:find ?e ?s :where [?e "salary" ?s] [(> ?s 50000)]]', db()))
  );
  bench(config, "qpred2", () =>
    consumeRows(d.q('[:find ?e ?s :in $ ?min_s :where [?e "salary" ?s] [(> ?s ?min_s)]]', db(), 50000))
  );
  bench(config, "pull-one", () =>
    consumePull(d.pull(db(), '["name" "age" {"friend" ["name" "age"]}]', 1))
  );
  console.error(`blackhole=${blackhole}`);
}

main();
