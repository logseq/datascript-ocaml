# Experiment backlog (Logseq-shaped workloads first)

Status: ( ) queued  (>) running  (k) kept  (x) reverted  (d) done/dry

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
