"use strict";

const path = require("path");
const d = require(path.resolve(process.cwd(), process.argv[2]));

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

for (const name of [
  "empty_db",
  "init_db",
  "q",
  "pull",
  "pull_many",
  "db_with",
  "create_conn",
  "conn_from_db",
  "conn_from_datoms",
  "db",
  "transact",
  "reset_conn",
  "resolve_tempid",
  "datoms",
  "seek_datoms",
  "index_range",
  "squuid",
  "squuid_time_millis",
]) {
  assert(typeof d[name] === "function", `missing JS facade export: ${name}`);
}

const schema = {
  name: { ":db/index": true, ":db/unique": ":db.unique/identity" },
  age: { ":db/index": true },
  friend: { ":db/valueType": ":db.type/ref" },
};

let db = d.empty_db(schema);
db = d.db_with(db, [
  { ":db/id": 1, name: "Ivan", age: 32 },
  { ":db/id": 2, name: "Oleg", age: 28, friend: 1 },
]);

assert(JSON.stringify(d.q("[:find ?name :where [1 :name ?name]]", db)) === JSON.stringify([["Ivan"]]), "q should return JS rows");
assert(d.datoms(db, ":eavt").length === 5, "datoms should return JS datom array");
assert(d.index_range(db, "age", 30, 40).length === 1, "index_range should return indexed datoms");
assert(d.pull(db, "[:name]", 1).name === "Ivan", "pull should return JS object attrs");
assert(d.pull_many(db, "[:name]", [1, 2]).length === 2, "pull_many should return JS array");

const conn = d.create_conn(schema);
const report = d.transact(conn, [{ ":db/id": -1, name: "Petr" }]);
assert(d.db(conn) === report.db_after, "transact should update the connection");
assert(d.resolve_tempid(report.tempids, -1) === 1, "resolve_tempid should read tempid map");

const beforeSquuid = Math.floor(Date.now() / 1000);
const squuid = d.squuid();
const afterSquuid = Math.floor(Date.now() / 1000);
const squuidSeconds = Number.parseInt(squuid.slice(0, 8), 16);
assert(
  squuidSeconds >= beforeSquuid && squuidSeconds <= afterSquuid,
  `squuid should embed wall-clock seconds, got ${squuidSeconds} outside [${beforeSquuid}, ${afterSquuid}]`,
);
assert(d.squuid_time_millis(squuid) === squuidSeconds * 1000, "squuid_time_millis should decode squuid seconds");

const refConn = d.create_conn({
  friend: { ":db/valueType": ":db.type/ref" },
  team: { ":db/valueType": ":db.type/ref", ":db/cardinality": ":db.cardinality/many" },
});
d.transact(refConn, [
  [":db/add", 1, "friend", 2],
  { ":db/id": 1, team: [2, 3] },
]);
assert(
  JSON.stringify(d.q("[:find ?friend ?team :where [1 :friend ?friend] [1 :team ?team]]", d.db(refConn))) ===
    JSON.stringify([
      [2, 2],
      [2, 3],
    ]),
  "JS facade should parse schema ref values and cardinality-many entity arrays",
);
