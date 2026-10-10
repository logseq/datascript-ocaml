# Experiment backlog (Logseq-shaped workloads first)

Status: ( ) queued  (>) running  (k) kept  (x) reverted  (d) done/dry

- r44 coll inputs ([?v ...] membership), r45 string/eq preds, r46 contains? on value vars, r47 ident-entity resolution + or-bound contains + cross component closure — property/task rule family covered end-to-end
- remaining uncovered: or branches mixing ground-resolved clauses with for_var patterns (branch-local ground eval), top-level or-join, untuple, recursive rules (parent/class-extends), pull-selector vars

## Transact path (page/block edits)
(d) T1: profile add-1/add-5 — where do the ~21ms go for a small tx?
      -> rounds 1-3: tempid machinery skip + small-tx early-out + new-entity
      fast emission: add-1 21.4 -> ~16.1ms (-25%)
(d) T2: datom staged-set dedup / avet duplicate tables — per-tx allocation.
      -> covered by r1 (empty-tx Hashtbl resets skipped)
(d) T3: index update path — pss add cost per datom vs batching inside one tx.
      -> pss fused no-split emit kept (r4, ~3-4%); bulk merge would change
      node layout = forbidden (parity hash)
(d) T4: tempid + lookup-ref resolution passes — extra map scans per tx.
      -> r1 skipped tempid machinery entirely for tempid-free txs
(k) T6: Retract(e,a,v) ops on fast path via staged (e,a) effective view —
      batched index refresh instead of per-op refresh (r5, retract+add -40%)
(x) T7: RetractEntity on fast path — staged bookkeeping costs more than the
      single batched refresh on 1-op txs (46us vs 17us); reverted (r6)
( ) T5: tx report / listen pipeline cost for Logseq listeners (if measurable).

## Query path (page load, outliner reads)
(k) Q7: find-shape coverage over the entity-set matcher — ALL done:
      r25-31 not/or/missing?/scalar/relation/prefilter arms;
      r32 ref-target join (-45%); r33 range preds (-97%);
      r34 cross-ref ValueIn (-71%); r35 bound value vars (-95%);
      r36 aggregates (-97%); r37-39 n-var relation finds incl. external
      entity var + row dedup (-76~97%); r40 value-var finds
      (?v/[?v...]/?v.) (-80%); r41 non-recursive rule inlining;
      r42 input-bound attr vars (-96%); r43 get-else-bound vars.
      Equivalence: fast-vs-generic Marshal row compare in lq_prof.

(d) Q1: get-page-data profile — entity materialization per block dominates;
      matches upstream shape, no cheap structural win at impl level
( ) Q2: datoms seek iteration — seq allocation overhead in Avet/Aevt slices
      (datoms-sex-aevt 0.018ms vs avet exact 0.0066ms gap).
( ) Q3: join loop — clause ordering / intermediate row materialization
      (q-name-age-sex 0.156ms; q4 0.221ms worst).
(d) Q4: entity/entity_attrs cost (0.927ms for all attrs) — lazy view seek
      batching, attr map construction. -> tight already, see Q1
( ) Q5: pull path — pulled_attrs assembly for page trees.
( ) Q6: time each query in test/logseq_queries.edn; attack the worst shape.

## Storage/persistence path (format frozen!)
(d) S1: storage-roundtrip 36–48ms — split: db_with rebuild ~21ms (covered by
      transact work) + encode ~7ms + restore ~0ms.
( ) S2: transact→flush path on sqlite conn (restore_probe counters clean).

## Cross-cutting
(k) X3: pss fused emit — add no-split emit (r4) + remove branch child
      replace without rebalance (r7, pin b4c97e3)
( ) X1: remove per-call string concat / Printf.sprintf in hot loops.
( ) X2: allocation reduction in datom/tuple construction on query path.
- [ ] Q7 (partially done: lean clause datoms + attr memo + const hoist; join already streaming-hash) where-eval for join+missing?/not/or patterns — q132/q176 ~10ms on 2k rows (5us/row bindings machinery), q188 3.1ms attr-attr join, q110 or-join 2.2ms, q184/186 rule queries ~2.6ms — identified by lq_prof (logseq_queries.edn timing, scratch probe)

## Status 2026-10-10b (rounds 11-15, query where-eval)

Kept: lean clause-eval datoms (r11), per-db attr memo (r12), const pure-clause
hoist (r13), e-bound attr probe lean path + stream_join fold (r15).
Revert: wildcard_shadowed_attrs memo (r14, no gain).

Current lq_prof top: q143 10.6ms (pull machinery ~3us/entity + ~2us collect/row;
structural floor without pull-assembly redesign), q176 6.3, q132 4.0, q160 3.9,
q184/186 2.6 (rules), q110 2.2 (or-join), q188 1.9.

Remaining candidates (diminishing): pulled_attrs boxing/dedupe/sort rework,
collect_find_specs pull fast path, or-join + rule machinery, T5 tx report,
X2 alloc trims. Join machinery is already streaming hash-join ~0.65us/datom;
bindings machinery ~0.8us/row is the sequential-eval floor.

## cljs upstream comparison (2026-10-10, size 2000 / 14.6k datoms, min-of-8)

Harness: bench/lq_upstream.js on _deps/datascript/release-js/datascript.js
v1.7.3 (node); same LCG-seeded synthetic Logseq db + same query shapes as
lq_prof. String attrs (datascript.js does not keywordize tx attrs); schema
properties need colon-prefixed keyword values (":db/index", ":db.type/ref").

OCaml vs cljs (native ocaml vs V8, apples-to-oranges runtime):
  bulk db_with 57 vs 305ms (5.4x)   add-1 15.7 vs 163ms (10x)
  retr+add 14.7 vs 182ms (12x)      pull-wild 5.9 vs 80ms (13.5x)
  q143 10.3 vs 45ms (4.4x)   q176 6.3 vs 60ms (9.5x)   q160 3.9 vs 23ms (5.9x)
  q132 4.0 vs 9.5ms (2.4x)   q188 1.9 vs 3.2ms (1.7x)   joinonly 1.9 vs 2.7ms
  q110 or-join real attr arg: 44us vs 0.9ms (20x, after r17)
Everything is faster than upstream; no case trails cljs.

Caveat discovered: lq_prof arg synthesis maps "?property-id" -> Int64
(suffix "id"), so the edn q110 number measured a degenerate non-string
attr scan (~2.2ms full eavt), not the real or-join. Real or-join timing
needs explicit inputs (see "q110 attr-arg" variant in lq_prof).

## Status 2026-10-10c (rounds 16-19)

Kept: sort-based pulled-attr dedupe (r16); per-row eval for projected
branches whose first clause binds its entity var (r17+r19 — covers Or,
SourceOr, OrJoin(-Required), SourceClause, NotJoin paths and rule bodies;
q110 real-attr or-join ~44us, 20x under cljs); resolved non-attr
constants in attr position answer empty instead of full eavt scan (r18).

Verdict after cljs compare: no workload trails upstream. Remaining pool
is pull-assembly boxing internals (q143 ~10ms, ~3us/entity floor) and
degenerate-input edges — all <20% projected.
- r50 q 入口接通 fast-path 阶梯（shadow 绑定）：所有 Datascript.q 调用方获得全部收益；+arity/not-boundness/or-free-vars 语义预检 + 反向 attr(_a) 求值。q176 -98% q188 -95% q132 -76% q186/q184 -40%
