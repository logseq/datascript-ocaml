# Logseq shared query bench (3-way)

- Size: 5000 entities / 500 pages
- CLJS: `@logseq/nbb-logseq#feat-db-v34` — PSS indexes + SQLite `kvs` IStorage
- OCaml main: PSS + SQLite blob kvs (working set in memory after restore)
- OCaml current: non-PSS durable SQLite Share indexes (live B-tree tables)
- Workload: Logseq `initial_data` hot paths

| query | cljs-nbb-logseq-pss | ocaml-main-pss | ocaml-current-non-pss |
| --- | ---: | ---: | ---: |
| `build-ms` | 4398.35 | 309.07 | 9560.89 |
| `restore-ms` | 1.40 | 0.060 | 0.038 |
| `disk-bytes` | 5267456 | 7356416 | 9834496 |
| `recent-pages` | 0.424 | 0.416 | 0.269 |
| `latest-journals` | 0.269 | 0.210 | 0.035 |
| `uuid-lookup` | 0.032 | 0.018 | 0.016 |
| `title-lookup` | 0.0087 | 0.0024 | 0.0017 |
| `children-by-parent` | 0.0079 | 0.0021 | 0.0024 |
| `blocks-by-page` | 0.0073 | 0.0024 | 0.0046 |
| `tags-scan` | 0.0079 | 0.0026 | 0.0067 |
| `eavt-entity` | 0.0029 | 0.0018 | 0.0047 |
| `entity-hydrate` | 0.022 | 0.011 | 0.013 |
| `q-updated-at-between` | 4.01 | 0.135 | 0.184 |
| `q-journal-pages` | 0.772 | 3.83 | 0.458 |
| `q-page-by-name` | 0.048 | 0.0054 | 0.0029 |

Times are median ms/op.

Artifacts:
- `bench-logseq-shared-cljs-nbb.txt` (cljs-nbb-logseq-pss)
- `bench-logseq-shared-ocaml-main.txt` (ocaml-main-pss)
- `bench-logseq-shared-ocaml-current.txt` (ocaml-current-non-pss)
