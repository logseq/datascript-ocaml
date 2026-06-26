#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <sqlite3.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct cached_db {
  char *path;
  sqlite3 *db;
  struct cached_db *next;
} cached_db;

static cached_db *db_cache = NULL;
static pthread_mutex_t db_cache_mutex = PTHREAD_MUTEX_INITIALIZER;

static void fail_sqlite(sqlite3 *db, const char *context)
{
  const char *message = db == NULL ? "unknown SQLite error" : sqlite3_errmsg(db);
  char buffer[1024];
  snprintf(buffer, sizeof(buffer), "SQLite error while running %s: %s", context, message);
  caml_failwith(buffer);
}

static void check_sqlite(sqlite3 *db, int rc, const char *context)
{
  if (rc != SQLITE_OK && rc != SQLITE_DONE && rc != SQLITE_ROW) {
    fail_sqlite(db, context);
  }
}

static sqlite3 *open_db(value raw_path)
{
  const char *path = String_val(raw_path);
  sqlite3 *db = NULL;

  pthread_mutex_lock(&db_cache_mutex);
  for (cached_db *entry = db_cache; entry != NULL; entry = entry->next) {
    if (strcmp(entry->path, path) == 0) {
      db = entry->db;
      pthread_mutex_unlock(&db_cache_mutex);
      return db;
    }
  }

  int rc = sqlite3_open(path, &db);
  if (rc != SQLITE_OK) {
    pthread_mutex_unlock(&db_cache_mutex);
    fail_sqlite(db, "open database");
  }

  cached_db *entry = malloc(sizeof(cached_db));
  if (entry == NULL) {
    pthread_mutex_unlock(&db_cache_mutex);
    caml_failwith("SQLite cache allocation failed");
  }
  entry->path = strdup(path);
  if (entry->path == NULL) {
    free(entry);
    pthread_mutex_unlock(&db_cache_mutex);
    caml_failwith("SQLite cache path allocation failed");
  }
  entry->db = db;
  entry->next = db_cache;
  db_cache = entry;
  pthread_mutex_unlock(&db_cache_mutex);
  return db;
}

static void close_db(sqlite3 *db)
{
  (void)db;
}

static void close_db_path(const char *path)
{
  pthread_mutex_lock(&db_cache_mutex);
  cached_db **cursor = &db_cache;
  while (*cursor != NULL) {
    cached_db *entry = *cursor;
    if (strcmp(entry->path, path) == 0) {
      *cursor = entry->next;
      pthread_mutex_unlock(&db_cache_mutex);
      sqlite3_close(entry->db);
      free(entry->path);
      free(entry);
      return;
    }
    cursor = &entry->next;
  }
  pthread_mutex_unlock(&db_cache_mutex);
}

static void exec_sql(sqlite3 *db, const char *sql)
{
  check_sqlite(db, sqlite3_exec(db, sql, NULL, NULL, NULL), sql);
}

static void ensure_schema(sqlite3 *db)
{
  exec_sql(
    db,
    "create table if not exists kvs "
    "(address text primary key not null, payload text not null);");
}

static value make_some(value payload)
{
  CAMLparam1(payload);
  CAMLlocal1(result);
  result = caml_alloc(1, 0);
  Store_field(result, 0, payload);
  CAMLreturn(result);
}

CAMLprim value datascript_sqlite_open(value raw_path)
{
  CAMLparam1(raw_path);
  sqlite3 *db = open_db(raw_path);
  ensure_schema(db);
  CAMLreturn(Val_unit);
}

CAMLprim value datascript_sqlite_close(value raw_path)
{
  CAMLparam1(raw_path);
  close_db_path(String_val(raw_path));
  CAMLreturn(Val_unit);
}

CAMLprim value datascript_sqlite_store(value raw_path, value raw_entries)
{
  CAMLparam2(raw_path, raw_entries);
  sqlite3 *db = open_db(raw_path);
  sqlite3_stmt *stmt = NULL;
  const char *sql = "replace into kvs (address, payload) values (?, ?);";

  ensure_schema(db);
  exec_sql(db, "begin immediate transaction;");
  if (sqlite3_prepare_v2(db, sql, -1, &stmt, NULL) != SQLITE_OK) {
    exec_sql(db, "rollback transaction;");
    fail_sqlite(db, sql);
  }

  for (value cursor = raw_entries; cursor != Val_emptylist; cursor = Field(cursor, 1)) {
    value pair = Field(cursor, 0);
    value address = Field(pair, 0);
    value payload = Field(pair, 1);
    sqlite3_reset(stmt);
    sqlite3_clear_bindings(stmt);
    check_sqlite(db, sqlite3_bind_text(stmt, 1, String_val(address), -1, SQLITE_TRANSIENT), sql);
    check_sqlite(
      db,
      sqlite3_bind_text(stmt, 2, String_val(payload), caml_string_length(payload), SQLITE_TRANSIENT),
      sql);
    check_sqlite(db, sqlite3_step(stmt), sql);
  }

  sqlite3_finalize(stmt);
  exec_sql(db, "commit transaction;");
  close_db(db);
  CAMLreturn(Val_unit);
}

CAMLprim value datascript_sqlite_restore(value raw_path, value raw_address)
{
  CAMLparam2(raw_path, raw_address);
  CAMLlocal2(payload, result);
  sqlite3 *db = open_db(raw_path);
  sqlite3_stmt *stmt = NULL;
  const char *sql = "select payload from kvs where address = ?;";

  ensure_schema(db);
  check_sqlite(db, sqlite3_prepare_v2(db, sql, -1, &stmt, NULL), sql);
  check_sqlite(db, sqlite3_bind_text(stmt, 1, String_val(raw_address), -1, SQLITE_TRANSIENT), sql);

  int rc = sqlite3_step(stmt);
  if (rc == SQLITE_ROW) {
    const unsigned char *text = sqlite3_column_text(stmt, 0);
    int bytes = sqlite3_column_bytes(stmt, 0);
    payload = caml_alloc_string(bytes);
    memcpy(Bytes_val(payload), text, bytes);
    result = make_some(payload);
  } else if (rc == SQLITE_DONE) {
    result = Val_none;
  } else {
    sqlite3_finalize(stmt);
    fail_sqlite(db, sql);
  }

  sqlite3_finalize(stmt);
  close_db(db);
  CAMLreturn(result);
}

CAMLprim value datascript_sqlite_list_addresses(value raw_path)
{
  CAMLparam1(raw_path);
  CAMLlocal3(result, cell, address);
  sqlite3 *db = open_db(raw_path);
  sqlite3_stmt *stmt = NULL;
  const char *sql = "select address from kvs order by address desc;";

  ensure_schema(db);
  check_sqlite(db, sqlite3_prepare_v2(db, sql, -1, &stmt, NULL), sql);
  result = Val_emptylist;
  while (true) {
    int rc = sqlite3_step(stmt);
    if (rc == SQLITE_DONE) {
      break;
    }
    if (rc != SQLITE_ROW) {
      sqlite3_finalize(stmt);
      fail_sqlite(db, sql);
    }
    address = caml_copy_string((const char *)sqlite3_column_text(stmt, 0));
    cell = caml_alloc(2, 0);
    Store_field(cell, 0, address);
    Store_field(cell, 1, result);
    result = cell;
  }

  sqlite3_finalize(stmt);
  close_db(db);
  CAMLreturn(result);
}

CAMLprim value datascript_sqlite_delete(value raw_path, value raw_addresses)
{
  CAMLparam2(raw_path, raw_addresses);
  sqlite3 *db = open_db(raw_path);
  sqlite3_stmt *stmt = NULL;
  const char *sql = "delete from kvs where address = ?;";

  ensure_schema(db);
  check_sqlite(db, sqlite3_prepare_v2(db, sql, -1, &stmt, NULL), sql);
  for (value cursor = raw_addresses; cursor != Val_emptylist; cursor = Field(cursor, 1)) {
    value address = Field(cursor, 0);
    sqlite3_reset(stmt);
    sqlite3_clear_bindings(stmt);
    check_sqlite(db, sqlite3_bind_text(stmt, 1, String_val(address), -1, SQLITE_TRANSIENT), sql);
    check_sqlite(db, sqlite3_step(stmt), sql);
  }

  sqlite3_finalize(stmt);
  close_db(db);
  CAMLreturn(Val_unit);
}
