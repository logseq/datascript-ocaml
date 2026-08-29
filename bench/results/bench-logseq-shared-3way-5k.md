# Logseq shared query bench (3-way)

- Size: 5000 entities / 500 pages
- CLJS: `@logseq/nbb-logseq#feat-db-v34` — PSS indexes + SQLite `kvs` IStorage (Logseq DB path)
- OCaml: non-PSS SQLite index store/restore (no PSS)
- Workload: Logseq `initial_data` hot paths

| query | cljs-nbb-logseq-pss | ocaml-main | ocaml-current |
| --- | ---: | ---: | ---: |
| `build-ms` | 3695.22 | 315.80 | 10273.47 |
| `restore-ms` | 1.74 | 0.063 | 0.039 |
| `disk-bytes` | 5267456 | 7356416 | 9834496 |
| `recent-pages` | 0.415 | 0.415 | 0.915 |
| `latest-journals` | 0.261 | 0.199 | 0.014 |
| `uuid-lookup` | 0.031 | 0.015 | 0.059 |
| `title-lookup` | 0.0086 | 0.0022 | 0.0053 |
| `children-by-parent` | 0.0079 | 0.0021 | 0.0060 |
| `blocks-by-page` | 0.0071 | 0.0024 | 0.0082 |
| `tags-scan` | 0.0079 | 0.0024 | 0.010 |
| `eavt-entity` | 0.0027 | 0.0017 | 0.0083 |
| `entity-hydrate` | 0.021 | 0.011 | 0.050 |
| `q-updated-at-between` | 4.07 | 0.133 | 0.031 |
| `q-journal-pages` | 0.786 | 3.75 | 0.463 |
| `q-page-by-name` | 0.045 | 0.0053 | 0.0069 |

Times are median ms/op.

Artifacts:
- `bench-logseq-shared-cljs-nbb.txt` (cljs-nbb-logseq-pss)
- `bench-logseq-shared-ocaml-main.txt` (ocaml-main)
- `bench-logseq-shared-ocaml-current.txt` (ocaml-current)
