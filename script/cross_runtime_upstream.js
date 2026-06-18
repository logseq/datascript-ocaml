#!/usr/bin/env node
"use strict";

const datascriptPath =
  process.env.UPSTREAM_DATASCRIPT_JS || "/Users/tiensonqin/Codes/projects/datascript/release-js/datascript.js";
const d = require(datascriptPath);

const tx0 = 0x20000000;

function sorted(value) {
  if (Array.isArray(value)) {
    return value.map(sorted);
  }
  if (value && typeof value === "object") {
    const out = {};
    for (const key of Object.keys(value).sort()) {
      out[key] = sorted(value[key]);
    }
    return out;
  }
  return value;
}

function emit(name, value) {
  console.log(`${name}\t${JSON.stringify(sorted(value))}`);
}

function datomValue(value) {
  return value;
}

function normalizeDatom(datom) {
  const tx = Math.abs(datom.tx == null ? tx0 : datom.tx);
  return [datom.e, datom.a, datomValue(datom.v), tx, datom.tx == null || datom.tx >= 0];
}

function normalizeDatoms(datoms) {
  return datoms.map(normalizeDatom);
}

function normalizeRows(rows) {
  return rows.slice().sort((left, right) => JSON.stringify(left).localeCompare(JSON.stringify(right)));
}

function normalizeTempids(tempids) {
  const out = {};
  for (const key of Object.keys(tempids).sort()) {
    out[key.replace(/^:/, "")] = tempids[key];
  }
  return out;
}

function normalizeSchema(serializedSchema) {
  const schemaText = String(serializedSchema);
  const entries = [];
  const entryPattern = /"([^"]+)"\s+\{([^}]*)\}/g;
  let match;
  while ((match = entryPattern.exec(schemaText)) !== null) {
    const attr = match[1];
    const body = match[2];
    entries.push([
      attr,
      {
        cardinality: body.includes(":db.cardinality/many") ? "many" : "one",
        unique: body.includes(":db.unique/identity")
          ? "identity"
          : body.includes(":db.unique/value")
            ? "value"
            : null,
        indexed: body.includes(":db/index true") || body.includes(":db/unique"),
        value_type: body.includes(":db.type/ref") ? "ref" : null,
      },
    ]);
  }
  return entries.sort((left, right) => left[0].localeCompare(right[0]));
}

function normalizeError(fn) {
  try {
    fn();
    return { outcome: "ok" };
  } catch (error) {
    const message = String(error && error.message ? error.message : error);
    return {
      outcome: "error",
      category: message.includes("unique constraint") ? "unique constraint" : message,
    };
  }
}

const schema = {
  name: { ":db/unique": ":db.unique/identity" },
  age: { ":db/index": true },
  friend: { ":db/valueType": ":db.type/ref" },
  aka: { ":db/cardinality": ":db.cardinality/many" },
};

const conn = d.create_conn(schema);
const firstReport = d.transact(conn, [
  { ":db/id": -1, name: "Ivan", age: 31, aka: ["Vanya", "I"], friend: -2 },
  { ":db/id": -2, name: "Petr", age: 44 },
]);
const firstDb = firstReport.db_after;

emit("schema", normalizeSchema(d.serializable(firstDb).schema));
emit("tx.first.tempids", normalizeTempids(firstReport.tempids));
emit("tx.first.datoms", normalizeDatoms(firstReport.tx_data));
emit("datoms.eavt.after_first", normalizeDatoms(d.datoms(firstDb, ":eavt")));
emit("query.names_ages", normalizeRows(d.q('[:find ?n ?a :where [?e "name" ?n] [?e "age" ?a]]', firstDb)));
emit("pull.friend", d.pull(firstDb, '["name" {"friend" ["name"]}]', 1));

const secondReport = d.transact(conn, [
  [":db/add", 1, "age", 32],
  [":db/retract", 1, "aka", "I"],
]);
const secondDb = secondReport.db_after;

emit("tx.second.datoms", normalizeDatoms(secondReport.tx_data));
emit("datoms.eavt.after_second", normalizeDatoms(d.datoms(secondDb, ":eavt")));
emit(
  "error.unique_value",
  normalizeError(() => {
    const errorConn = d.create_conn({ email: { ":db/unique": ":db.unique/value" } });
    d.transact(errorConn, [
      { ":db/id": 1, email: "a@example.test" },
      { ":db/id": 2, email: "a@example.test" },
    ]);
  })
);
