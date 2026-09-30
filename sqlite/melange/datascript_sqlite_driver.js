/**
 * Host-installed sync SQLite driver for Datascript Melange.
 *
 * This module does not choose a runtime (worker, window, Node, ...). The host
 * calls `setDriver` with a connection object that talks to sqlite-wasm,
 * node:sqlite, or any other synchronous SQLite.
 *
 * Driver methods:
 *   open(path) -> conn
 *   close(conn)
 *   exec(conn, sql)
 *   metaGet(conn, key) -> string | null | undefined
 *   metaSet(conn, key, value)
 *   put(conn, table, key, value)
 *   putMany(conn, table, entries) where entries is [key, value][]
 *   get(conn, table, key) -> string | null | undefined
 *   remove(conn, table, key)
 *   fold(conn, table, fromKey, desc, fn)
 *     fromKey: string | null, desc: boolean
 *     fn(key, value) -> true to stop
 *   begin(conn)
 *   commit(conn)
 *   rollback(conn)
 *
 * Keys and values are binary JS strings (one byte per char), matching OCaml
 * `string` blobs.
 */
let driver = null;

export function setDriver(next) {
  driver = next;
}

export function hasDriver() {
  return driver != null;
}

function requireDriver() {
  if (!driver) {
    throw new Error("Datascript SQLite driver is not installed");
  }
  return driver;
}

export function sqlOpen(path) {
  return requireDriver().open(path);
}

export function sqlClose(conn) {
  requireDriver().close(conn);
}

export function sqlExec(conn, sql) {
  requireDriver().exec(conn, sql);
}

export function sqlMetaGet(conn, key) {
  const value = requireDriver().metaGet(conn, key);
  return value == null ? undefined : value;
}

export function sqlMetaSet(conn, key, value) {
  requireDriver().metaSet(conn, key, value);
}

export function sqlPut(conn, table, key, value) {
  requireDriver().put(conn, table, key, value);
}

export function sqlPutMany(conn, table, entries) {
  const impl = requireDriver();
  if (typeof impl.putMany === "function") {
    impl.putMany(conn, table, entries);
    return;
  }
  for (let i = 0; i < entries.length; i += 1) {
    const pair = entries[i];
    impl.put(conn, table, pair[0], pair[1]);
  }
}

export function sqlGet(conn, table, key) {
  const value = requireDriver().get(conn, table, key);
  return value == null ? undefined : value;
}

export function sqlRemove(conn, table, key) {
  requireDriver().remove(conn, table, key);
}

export function sqlFold(conn, table, fromKey, desc, fn) {
  const start = fromKey === undefined ? null : fromKey;
  requireDriver().fold(conn, table, start, desc, fn);
}

export function sqlBegin(conn) {
  requireDriver().begin(conn);
}

export function sqlCommit(conn) {
  requireDriver().commit(conn);
}

export function sqlRollback(conn) {
  requireDriver().rollback(conn);
}
