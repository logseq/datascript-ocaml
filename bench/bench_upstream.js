#!/usr/bin/env node
"use strict";

const path = require("path");

const datascriptPath = process.env.UPSTREAM_DATASCRIPT_JS;

if (!datascriptPath) {
  console.error("Set UPSTREAM_DATASCRIPT_JS to the upstream DataScript JS bundle.");
  process.exit(2);
}

const d = require(path.resolve(datascriptPath));

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
const aliases = [
  "A. C. Q. W.",
  "A. J. Finn",
  "A.A. Fair",
  "Aapeli",
  "Aaron Wolfe",
  "Abigail Van Buren",
  "Jeanne Phillips",
  "Abram Tertz",
  "Abu Nuwas",
  "Acton Bell",
  "Adunis",
];

function makeRng(seed) {
  let state = seed | 0;
  return function nextInt(bound) {
    state = (Math.imul(state, 1664525) + 1013904223) | 0;
    return ((state >>> 1) & 0x3fffffff) % bound;
  };
}

function randNth(nextInt, values) {
  return values[nextInt(values.length)];
}

function randomMan(nextInt, i) {
  const name = randNth(nextInt, names);
  const lastName = randNth(nextInt, lastNames);
  const aliasCount = nextInt(10);
  const aliasValues = [];
  for (let index = 0; index < aliasCount; index += 1) {
    aliasValues.push(randNth(nextInt, aliases));
  }
  return {
    ":db/id": String(i),
    name,
    "last-name": lastName,
    "full-name": `${name} ${lastName}`,
    alias: aliasValues,
    sex: nextInt(2) === 0 ? ":male" : ":female",
    age: nextInt(100),
    salary: nextInt(100000),
  };
}

function people(size) {
  const nextInt = makeRng(1);
  const result = [];
  for (let i = 1; i <= size; i += 1) {
    result.push(randomMan(nextInt, i));
  }
  return result;
}

function buildDb(size) {
  return d.db_with(d.empty_db(schema), people(size));
}

// Logseq get-page-data scenario — mirrors
// src/main/logseq/api/db_based/tools.cljs (get-page-data -> get-page-blocks ->
// otree/blocks->vec-tree): page lookup via avet :block/name, block listing via
// avet :block/page, full entity materialization, then nesting by :block/parent
// sorted on :block/order.

const logseqSchema = {
  "block/name": { ":db/index": true },
  "block/title": { ":db/index": true },
  "block/uuid": { ":db/unique": ":db.unique/identity" },
  "block/page": { ":db/valueType": ":db.type/ref", ":db/index": true },
  "block/parent": { ":db/valueType": ":db.type/ref", ":db/index": true },
  "block/order": { ":db/index": true },
  "block/journal-day": { ":db/index": true },
  "block/refs": { ":db/valueType": ":db.type/ref", ":db/cardinality": ":db.cardinality/many" },
  "block/tags": { ":db/valueType": ":db.type/ref", ":db/cardinality": ":db.cardinality/many" },
};

const logseqBlocksPerPage = 100;

function logseqPageName(i) {
  return `page-${String(i).padStart(6, "0")}`;
}

function logseqBlockTitle(nextInt, i) {
  return `block ${i} content [[some page]] #tag and a ref to ((` +
    `${nextInt(0xffffffff).toString(16).padStart(8, "0")}-${nextInt(0xffff).toString(16).padStart(4, "0")}))`;
}

function logseqData(size) {
  const nextInt = makeRng(1);
  const numPages = Math.max(1, Math.floor(size / logseqBlocksPerPage));
  const blocksPerPage = Math.floor(size / numPages);
  let nextId = 0;
  const freshId = () => String(++nextId);
  const tagIds = Array.from({ length: 5 }, () => freshId());
  const txData = tagIds.map((id) => ({
    ":db/id": id,
    "block/title": `tag-${id}`,
    "block/uuid": `tag-uuid-${id}`,
  }));
  for (let p = 0; p < numPages; p += 1) {
    const pageId = freshId();
    const page = {
      ":db/id": pageId,
      "block/name": logseqPageName(p),
      "block/title": logseqPageName(p),
      "block/uuid": `page-uuid-${String(p).padStart(6, "0")}`,
      "block/created-at": 1700000000000,
      "block/updated-at": 1700000100000,
    };
    if (p % 2 === 0) page["block/journal-day"] = 20260101;
    txData.push(page);
    const siblingCounts = new Map();
    const nextOrder = (parent) => {
      const n = siblingCounts.get(parent) || 0;
      siblingCounts.set(parent, n + 1);
      return String(n).padStart(8, "0");
    };
    for (let b = 0; b < blocksPerPage; b += 1) {
      const blockId = freshId();
      // ~40% top-level, rest nested under a random earlier block
      const parent = b === 0 || b % 5 < 2
        ? pageId
        : String(Number(pageId) + 1 + nextInt(b));
      const block = {
        ":db/id": blockId,
        "block/uuid": `block-uuid-${blockId}`,
        "block/title": logseqBlockTitle(nextInt, b),
        "block/page": Number(pageId),
        "block/parent": Number(parent),
        "block/order": nextOrder(parent),
        "block/created-at": 1700000000000,
        "block/updated-at": 1700000100000,
      };
      if (b % 10 === 0) block["block/tags"] = [Number(randNth(nextInt, tagIds))];
      if (b % 7 === 0) block["block/refs"] = [6 + nextInt(numPages)];
      txData.push(block);
    }
  }
  return txData;
}

function buildLogseqDb(size) {
  return d.db_with(d.empty_db(logseqSchema), logseqData(size));
}

function materializeEntity(db, eid) {
  // (into {} (d/entity db eid)): iterate eavt attrs, resolving through the
  // entity so ref values come back as entities
  const entity = d.entity(db, eid);
  const map = { "db/id": eid };
  for (const dtm of d.datoms(db, ":eavt", eid)) {
    map[dtm.a] = entity.get(dtm.a);
  }
  return map;
}

function logseqGetPageData(db, name) {
  const pageHits = d.datoms(db, ":avet", "block/name", name);
  if (!pageHits.length) return 0;
  const pageId = pageHits[0].e;
  const page = materializeEntity(db, pageId);
  let count = Object.keys(page).length;
  const blockEids = d.datoms(db, ":avet", "block/page", pageId).map((x) => x.e);
  const blocks = blockEids.map((eid) => {
    const block = materializeEntity(db, eid);
    count += Object.keys(block).length;
    const parentEntity = block["block/parent"];
    block._parentEid = parentEntity && parentEntity.eid !== undefined ? parentEntity.eid : pageId;
    return block;
  });
  // otree/blocks->vec-tree: group-by :block/parent, sort by :block/order, nest
  const childrenOf = new Map();
  for (const block of blocks) {
    const parent = block._parentEid;
    let siblings = childrenOf.get(parent);
    if (!siblings) childrenOf.set(parent, (siblings = []));
    siblings.push(block);
  }
  for (const siblings of childrenOf.values()) {
    siblings.sort((a, b) => (a["block/order"] < b["block/order"] ? -1 : 1));
  }
  const countSubtree = (parent) => {
    let n = 0;
    for (const block of childrenOf.get(parent) || []) {
      n += 1 + countSubtree(block["db/id"]);
    }
    return n;
  };
  return count + countSubtree(pageId);
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
  const attrs = ["name", "last-name", "sex", "age", "salary"];
  for (const entity of people(size)) {
    const id = entity[":db/id"];
    for (const attr of attrs) {
      db = d.db_with(db, [[":db/add", id, attr, entity[attr]]]);
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
  bench(config, "q2pred", () =>
    consumeRows(d.q('[:find ?e ?s :where [?e "name" "Ivan"] [?e "salary" ?s] [(> ?s 50000)]]', db()))
  );
  bench(config, "pull-one", () =>
    consumePull(d.pull(db(), '["name" "age" {"friend" ["name" "age"]}]', 1))
  );
  let cachedLogseqDb = null;
  const logseqDb = () => {
    if (cachedLogseqDb === null) cachedLogseqDb = buildLogseqDb(config.size);
    return cachedLogseqDb;
  };
  const logseqMidPage = logseqPageName(
    Math.floor(Math.max(1, Math.floor(config.size / logseqBlocksPerPage)) / 2)
  );
  bench(config, "get-page-data", () => consumeInt(logseqGetPageData(logseqDb(), logseqMidPage)));
  console.error(`blackhole=${blackhole}`);
}

main();
