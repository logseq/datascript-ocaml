"use strict";

const fs = require("fs");
const os = require("os");
const path = require("path");
const crypto = require("crypto");
const childProcess = require("child_process");
const { DatabaseSync } = require("node:sqlite");

const nativeExe = path.resolve(process.argv[2]);
const jsOfOcamlPath = path.resolve(process.argv[3]);
const upstreamPath = path.resolve(process.env.UPSTREAM_DATASCRIPT_JS || process.argv[4] || "../datascript/release-js/datascript.js");

const actionCount = 1000;
const batchSize = 20;
const refAttrs = new Set(["block/page", "block/parent", "block/refs", "block/tags", "property/reviewer"]);

function schema() {
  return {
    "block/uuid": { ":db/unique": ":db.unique/identity" },
    "block/name": { ":db/index": true },
    "block/title": { ":db/index": true },
    "block/page": { ":db/valueType": ":db.type/ref" },
    "block/parent": { ":db/valueType": ":db.type/ref" },
    "block/refs": { ":db/valueType": ":db.type/ref", ":db/cardinality": ":db.cardinality/many" },
    "block/tags": { ":db/valueType": ":db.type/ref", ":db/cardinality": ":db.cardinality/many" },
    "db/ident": { ":db/unique": ":db.unique/identity" },
    "property/type": { ":db/index": true },
    "property/public?": { ":db/index": true },
    "property/default-value": { ":db/index": true },
    "property/status": { ":db/index": true },
    "property/priority": { ":db/index": true },
    "property/estimate": { ":db/index": true },
    "property/reviewer": { ":db/valueType": ":db.type/ref" },
    "property/labels": { ":db/cardinality": ":db.cardinality/many" },
  };
}

function mulberry32(seed) {
  let state = seed >>> 0;
  return () => {
    state = (state + 0x6d2b79f5) >>> 0;
    let t = state;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

const rand = mulberry32(0x5eed);
const int = (n) => Math.floor(rand() * n);
const bool = () => int(2) === 0;
const choice = (values) => values[int(values.length)];
const pageId = () => 1 + int(20);
const propertyId = () => 100 + int(16);
const blockId = () => 1000 + int(500);
const labelValue = () => `label-${int(24)}`;

function blockName(title) {
  return title.toLowerCase().replaceAll(" ", "-");
}

function createPageTx(page) {
  const title = `Page ${page}`;
  return [
    [":db/add", page, "block/uuid", `page-${page}`],
    [":db/add", page, "block/name", blockName(title)],
    [":db/add", page, "block/title", title],
  ];
}

function createPropertyTx(property) {
  return [
    [":db/add", property, "db/ident", `:property/generated-${property}`],
    [":db/add", property, "property/type", ":default"],
    [":db/add", property, "property/public?", true],
    [":db/add", property, "property/default-value", ""],
    [":db/add", property, "block/title", `Generated property ${property}`],
  ];
}

function createBlockTx(revision) {
  const block = blockId();
  const title = `Created block ${block} rev ${revision}`;
  const page = pageId();
  const parent = bool() ? page : blockId();
  return [
    [":db/add", block, "block/uuid", `block-${block}`],
    [":db/add", block, "block/name", blockName(title)],
    [":db/add", block, "block/title", title],
    [":db/add", block, "block/page", page],
    [":db/add", block, "block/parent", parent],
    [":db/add", block, "block/refs", pageId()],
    [":db/add", block, "block/refs", propertyId()],
    [":db/add", block, "block/tags", propertyId()],
    [":db/add", block, "property/status", choice([":todo", ":doing", ":done"])],
    [":db/add", block, "property/priority", 1 + int(5)],
    [":db/add", block, "property/labels", labelValue()],
  ];
}

function updateBlockTx(revision) {
  const block = blockId();
  const title = `Updated block ${block} rev ${revision}`;
  switch (int(7)) {
    case 0:
      return [
        [":db/add", block, "block/title", title],
        [":db/add", block, "block/name", blockName(title)],
      ];
    case 1:
      return [
        [":db/add", block, "block/page", pageId()],
        [":db/add", block, "block/parent", blockId()],
      ];
    case 2:
      return [
        [":db/add", block, "block/refs", pageId()],
        [":db/add", block, "block/tags", propertyId()],
      ];
    case 3:
      return [
        [":db/retract", block, "block/refs", pageId()],
        [":db/retract", block, "block/tags", propertyId()],
      ];
    case 4:
      return [
        [":db/add", block, "property/status", choice([":todo", ":doing", ":done", ":blocked"])],
        [":db/add", block, "property/priority", 1 + int(5)],
        [":db/add", block, "property/estimate", int(21)],
      ];
    case 5:
      return [
        [":db/add", block, "property/reviewer", blockId()],
        [":db/add", block, "property/labels", labelValue()],
      ];
    default:
      return [
        [":db/retract", block, "property/status"],
        [":db/retract", block, "property/labels", labelValue()],
      ];
  }
}

function updatePropertyTx() {
  const property = propertyId();
  switch (int(4)) {
    case 0:
      return createPropertyTx(property);
    case 1:
      return [
        [":db/add", property, "property/type", choice([":default", ":number", ":date", ":checkbox"])],
        [":db/add", property, "property/public?", bool()],
      ];
    case 2:
      return [[":db/add", property, "property/default-value", `default-${int(128)}`]];
    default:
      return [[":db.fn/retractEntity", property]];
  }
}

function deleteBlockTx() {
  const block = blockId();
  switch (int(3)) {
    case 0:
      return [[":db.fn/retractEntity", block]];
    case 1:
      return [
        [":db/retract", block, "block/parent"],
        [":db/retract", block, "block/page"],
      ];
    default:
      return [
        [":db/retract", block, "property/priority"],
        [":db/retract", block, "property/estimate"],
        [":db/retract", block, "property/reviewer"],
      ];
  }
}

function randomGraphTx(index) {
  switch (int(10)) {
    case 0:
      return createPageTx(pageId());
    case 1:
      return updatePropertyTx();
    case 2:
    case 3:
      return createBlockTx(index);
    case 4:
      return deleteBlockTx();
    default:
      return updateBlockTx(index);
  }
}

function generatedBatches() {
  const batches = [];
  for (let i = 1; i <= 20; i += 1) batches.push(createPageTx(i));
  for (let i = 0; i < 16; i += 1) batches.push(createPropertyTx(100 + i));
  let ops = [];
  for (let index = 0; ops.length < actionCount; index += 1) {
    ops.push(...randomGraphTx(index));
  }
  ops = ops.slice(0, actionCount);
  for (let offset = 0; offset < ops.length; offset += batchSize) {
    batches.push(ops.slice(offset, offset + batchSize));
  }
  return batches;
}

function attrName(attr) {
  return typeof attr === "string" && attr.startsWith(":") ? attr.slice(1) : String(attr);
}

function datomField(datom, name, index) {
  if (Array.isArray(datom)) return datom[index];
  if (Object.prototype.hasOwnProperty.call(datom, name)) return datom[name];
  const keywordName = `:${name}`;
  if (Object.prototype.hasOwnProperty.call(datom, keywordName)) return datom[keywordName];
  return datom[name];
}

function canonicalValue(attr, value) {
  if (value === null || value === undefined) return "nil";
  if (typeof value === "number") return refAttrs.has(attr) ? `ref:${value}` : `int:${value}`;
  if (typeof value === "boolean") return `bool:${value}`;
  if (typeof value === "string") {
    return value.startsWith(":") ? `keyword:${value.slice(1)}` : `string:${value}`;
  }
  return "compound";
}

function canonicalDatomLine(datom) {
  const e = datomField(datom, "e", 0);
  const a = attrName(datomField(datom, "a", 1));
  const v = datomField(datom, "v", 2);
  const tx = datomField(datom, "tx", 3);
  const added = datomField(datom, "added", 4);
  return `datom\t${e}\t${a}\t${canonicalValue(a, v)}\t${tx}\t${added === false ? "false" : "true"}`;
}

function runRuntime(d, label, batches) {
  let conn = d.create_conn(schema());
  for (const batch of batches) {
    d.transact(conn, batch);
  }
  const lines = d.datoms(d.db(conn), ":eavt").map(canonicalDatomLine).sort();
  return { label, lines };
}

function writeLines(file, lines) {
  fs.writeFileSync(file, `${lines.join("\n")}\n`);
}

function sqlQuote(text) {
  return `'${String(text).replaceAll("'", "''")}'`;
}

function writeSqlite(file, lines) {
  if (fs.existsSync(file)) fs.rmSync(file);
  const db = new DatabaseSync(file);
  try {
    db.exec("pragma journal_mode = delete;");
    db.exec("pragma page_size = 4096;");
    db.exec("vacuum;");
    db.exec("create table kvs (addr INTEGER primary key, content TEXT, addresses JSON);");
    db.exec("begin immediate;");
    db.exec("insert into kvs (addr, content, addresses) values (0, 'datascript-sqlite-parity-v1', '[]');");
    lines.forEach((line, index) => {
      db.exec(`insert into kvs (addr, content, addresses) values (${index + 1}, ${sqlQuote(line)}, '[]');`);
    });
    db.exec("commit;");
    db.exec("vacuum;");
  } finally {
    db.close();
  }
}

function sqliteStats(file) {
  const db = new DatabaseSync(file);
  try {
    const row = db.prepare(
      "select count(*) rows, coalesce(sum(length(content)), 0) content_bytes, coalesce(sum(length(addresses)), 0) addresses_bytes from kvs;",
    ).get();
    return {
      rows: Number(row.rows),
      content_bytes: Number(row.content_bytes),
      addresses_bytes: Number(row.addresses_bytes),
      file_bytes: fs.statSync(file).size,
    };
  } finally {
    db.close();
  }
}

function sha256(file) {
  return crypto.createHash("sha256").update(fs.readFileSync(file)).digest("hex");
}

function assertEqual(label, expected, actual) {
  if (JSON.stringify(expected) !== JSON.stringify(actual)) {
    throw new Error(`${label}: expected ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`);
  }
}

function main() {
  const tmp = fs.mkdtempSync(path.join(os.tmpdir(), "datascript-sqlite-cross-runtime-"));
  try {
    const batches = generatedBatches();
    const input = { schema: schema(), batches };
    const inputFile = path.join(tmp, "input.json");
    fs.writeFileSync(inputFile, JSON.stringify(input));

    const nativeDatoms = path.join(tmp, "native.datoms");
    const nativeSqlite = path.join(tmp, "native.sqlite");
    childProcess.execFileSync(nativeExe, ["--input", inputFile, "--sqlite", nativeSqlite, "--datoms", nativeDatoms], {
      stdio: "inherit",
    });

    const runtimes = [
      { label: "js_of_ocaml", module: require(jsOfOcamlPath) },
      { label: "upstream-cljs", module: require(upstreamPath) },
    ];
    const results = [
      { label: "native", datoms: nativeDatoms, sqlite: nativeSqlite },
    ];

    for (const runtime of runtimes) {
      const result = runRuntime(runtime.module, runtime.label, batches);
      const datomsFile = path.join(tmp, `${runtime.label}.datoms`);
      const sqliteFile = path.join(tmp, `${runtime.label}.sqlite`);
      writeLines(datomsFile, result.lines);
      writeSqlite(sqliteFile, result.lines);
      results.push({ label: runtime.label, datoms: datomsFile, sqlite: sqliteFile });
    }

    const summary = results.map((result) => ({
      runtime: result.label,
      datoms: fs.readFileSync(result.datoms, "utf8").trimEnd().split("\n").filter(Boolean).length,
      hash: sha256(result.datoms),
      sqlite: sqliteStats(result.sqlite),
    }));

    const expectedHash = summary[0].hash;
    const expectedDatoms = summary[0].datoms;
    const expectedSqlite = summary[0].sqlite;
    for (const row of summary) {
      assertEqual(`${row.runtime} datom count`, expectedDatoms, row.datoms);
      assertEqual(`${row.runtime} datom hash`, expectedHash, row.hash);
      assertEqual(`${row.runtime} sqlite size`, expectedSqlite, row.sqlite);
    }

    console.log("runtime\tdatoms\thash\trows\tcontent_bytes\taddresses_bytes\tfile_bytes");
    for (const row of summary) {
      console.log(`${row.runtime}\t${row.datoms}\t${row.hash}\t${row.sqlite.rows}\t${row.sqlite.content_bytes}\t${row.sqlite.addresses_bytes}\t${row.sqlite.file_bytes}`);
    }
  } finally {
    if (process.env.DATASCRIPT_KEEP_PARITY_TMP !== "1") {
      fs.rmSync(tmp, { recursive: true, force: true });
    }
  }
}

main();
