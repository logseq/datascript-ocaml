// Node LMDB bindings for Melange. Requires the `lmdb` npm package.
const { open } = require("lmdb");
const fs = require("fs");
const os = require("os");
const path = require("path");

function removePath(dbPath) {
  if (fs.existsSync(dbPath)) fs.rmSync(dbPath, { recursive: true, force: true });
}

exports.open = function (dbPath) {
  removePath(dbPath);
  return open({ path: dbPath, compression: false });
};

exports.openDB = function (root, name) {
  return root.openDB(name, {});
};

exports.get = function (db, key) {
  return db.get(key);
};

exports.put = function (db, key, value) {
  db.put(key, value);
};

exports.remove = function (db, key) {
  db.remove(key);
};

exports.sync = function (_root) {};

exports.close = function (root) {
  root.close();
};

exports.range = function (db) {
  const entries = [];
  for (const { key, value } of db.getRange()) {
    entries.push([key, value]);
  }
  return entries;
};

exports.tempPath = function () {
  return path.join(
    os.tmpdir(),
    "datascript_lmdb_" + Date.now() + "_" + Math.random().toString(16).slice(2)
  );
};
