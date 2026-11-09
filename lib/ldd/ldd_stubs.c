/* The C side of lib/ldd: see ldd.mli.

   - Every call that makes nodes runs inside a Lace task (RUN): Sylvan's
     node table needs a worker's context. Sylvan's own operations are
     already tasks; [lddmc_cube] and [lddmc_union_cube] are wrapped here.
   - An OCaml value holds a node with [lddmc_ref], released by its
     finaliser or, earlier, by [ml_ldd_release]. A released value holds
     [lddmc_false], which needs no reference. */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sylvan.h>
#include <sylvan_int.h> /* the node table, [nodes] */
#include <caml/mlvalues.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/bigarray.h>

#define Mdd_val(v) (*((MDD *) Data_custom_val(v)))
#define MAX_WIDTH 4096

static void ldd_finalize(value v)
{
  if (Mdd_val(v) != lddmc_false) lddmc_deref(Mdd_val(v));
}

static int ldd_compare(value a, value b)
{
  MDD x = Mdd_val(a), y = Mdd_val(b);
  return x < y ? -1 : x > y ? 1 : 0;
}

static intnat ldd_hash(value v) { return (intnat) Mdd_val(v); }

static struct custom_operations ldd_ops = {
  "mebi.ldd", ldd_finalize, ldd_compare, ldd_hash,
  custom_serialize_default, custom_deserialize_default,
  custom_compare_ext_default, custom_fixed_length_default };

static value wrap(MDD d)
{
  /* each value keeps nodes alive outside OCaml's heap: tell the GC */
  value v = caml_alloc_custom_mem(&ldd_ops, sizeof(MDD), 4096);
  Mdd_val(v) = lddmc_ref(d);
  return v;
}

static size_t to_u32(value arr, uint32_t *out)
{
  size_t n = Wosize_val(arr);
  if (n > MAX_WIDTH) caml_invalid_argument("Ldd: vector too wide");
  for (size_t i = 0; i < n; i++) out[i] = (uint32_t) Long_val(Field(arr, i));
  return n;
}

TASK_2(MDD, cube_task, uint32_t *, vals, size_t, n) { return lddmc_cube(vals, n); }

TASK_4(MDD, union_cubes_task, MDD, set, value, rows, size_t, width, uint32_t *, buf)
{
  MDD r = set;
  lddmc_refs_pushptr(&r);
  size_t n = Wosize_val(rows);
  for (size_t i = 0; i < n; i++) {
    value row = Field(rows, i);
    for (size_t j = 0; j < width; j++) buf[j] = (uint32_t) Long_val(Field(row, j));
    r = lddmc_union_cube(r, buf, width);
  }
  lddmc_refs_popptr(1);
  return r;
}

static int started = 0;

value ml_ldd_init(value workers, value mb)
{
  if (!started) {
    lace_start(Int_val(workers), 1000000);
    sylvan_set_limits((size_t) Long_val(mb) * 1024 * 1024, 1, 5);
    sylvan_init_package();
    sylvan_init_ldd();
    started = 1;
  }
  return Val_unit;
}

value ml_ldd_false(value unit) { return wrap(lddmc_false); }
value ml_ldd_true(value unit) { return wrap(lddmc_true); }

value ml_ldd_cube(value arr)
{
  CAMLparam1(arr);
  uint32_t vals[MAX_WIDTH];
  size_t n = to_u32(arr, vals);
  CAMLreturn(wrap(RUN(cube_task, vals, n)));
}

value ml_ldd_union_cubes(value set, value rows)
{
  CAMLparam2(set, rows);
  static uint32_t buf[MAX_WIDTH];
  size_t width = Wosize_val(rows) > 0 ? Wosize_val(Field(rows, 0)) : 0;
  if (width > MAX_WIDTH) caml_invalid_argument("Ldd: vector too wide");
  CAMLreturn(wrap(RUN(union_cubes_task, Mdd_val(set), rows, width, buf)));
}

value ml_ldd_union(value a, value b)
{
  CAMLparam2(a, b);
  CAMLreturn(wrap(lddmc_union(Mdd_val(a), Mdd_val(b))));
}

value ml_ldd_minus(value a, value b)
{
  CAMLparam2(a, b);
  CAMLreturn(wrap(lddmc_minus(Mdd_val(a), Mdd_val(b))));
}

value ml_ldd_intersect(value a, value b)
{
  CAMLparam2(a, b);
  CAMLreturn(wrap(lddmc_intersect(Mdd_val(a), Mdd_val(b))));
}

value ml_ldd_relprod(value s, value r, value m)
{
  CAMLparam3(s, r, m);
  CAMLreturn(wrap(lddmc_relprod(Mdd_val(s), Mdd_val(r), Mdd_val(m))));
}

value ml_ldd_index(value s)
{
  return Val_long((long) Mdd_val(s));
}

value ml_ldd_relprev(value s, value r, value m, value u)
{
  CAMLparam4(s, r, m, u);
  CAMLreturn(wrap(lddmc_relprev(Mdd_val(s), Mdd_val(r), Mdd_val(m), Mdd_val(u))));
}

value ml_ldd_project(value s, value m)
{
  CAMLparam2(s, m);
  CAMLreturn(wrap(lddmc_project(Mdd_val(s), Mdd_val(m))));
}

value ml_ldd_count(value a) { return caml_copy_double((double) lddmc_satcount(Mdd_val(a))); }

value ml_ldd_nodes(value a) { return Val_long((long) lddmc_nodecount(Mdd_val(a))); }

value ml_ldd_equal(value a, value b) { return Val_bool(Mdd_val(a) == Mdd_val(b)); }

value ml_ldd_mem(value a, value arr)
{
  uint32_t vals[MAX_WIDTH];
  size_t n = to_u32(arr, vals);
  return Val_bool(lddmc_member_cube(Mdd_val(a), vals, n));
}

value ml_ldd_release(value v)
{
  if (Mdd_val(v) != lddmc_false) {
    lddmc_deref(Mdd_val(v));
    Mdd_val(v) = lddmc_false;
  }
  return Val_unit;
}

/* enumeration: a walk over the nodes (reads only), at most [limit] rows */

struct rows { uint32_t *data; size_t width, count, cap, limit; };

static void walk(MDD m, uint32_t *path, size_t depth, struct rows *r)
{
  if (r->count >= r->limit || m == lddmc_false) return;
  if (m == lddmc_true) {
    if (r->count == r->cap) {
      r->cap = r->cap ? 2 * r->cap : 64;
      r->data = realloc(r->data, r->cap * (depth ? depth : 1) * sizeof(uint32_t));
    }
    memcpy(r->data + r->count * (depth ? depth : 1), path, depth * sizeof(uint32_t));
    r->width = depth;
    r->count++;
    return;
  }
  for (MDD x = m; x != lddmc_false && r->count < r->limit; x = lddmc_getright(x)) {
    if (depth >= MAX_WIDTH) return;
    path[depth] = lddmc_getvalue(x);
    walk(lddmc_getdown(x), path, depth + 1, r);
  }
}

value ml_ldd_elements(value a, value limit)
{
  CAMLparam2(a, limit);
  CAMLlocal3(res, row, cell);
  static uint32_t path[MAX_WIDTH];
  struct rows r = { NULL, 0, 0, 0, (size_t) Long_val(limit) };
  walk(Mdd_val(a), path, 0, &r);
  res = Val_emptylist;
  for (size_t i = r.count; i > 0; i--) {
    row = caml_alloc(r.width, 0);
    for (size_t j = 0; j < r.width; j++)
      Store_field(row, j, Val_long(r.data[(i - 1) * (r.width ? r.width : 1) + j]));
    cell = caml_alloc(2, 0);
    Store_field(cell, 0, row);
    Store_field(cell, 1, res);
    res = cell;
  }
  free(r.data);
  CAMLreturn(res);
}

/* every vector, in one flat array of 32-bit values (row after row, in
   the diagram's order, which is lexicographic): for sets too large to hold
   as OCaml arrays */
value ml_ldd_elements_flat(value a)
{
  CAMLparam1(a);
  CAMLlocal2(res, ba);
  static uint32_t path[MAX_WIDTH];
  struct rows r = { NULL, 0, 0, 0, (size_t) -1 };
  walk(Mdd_val(a), path, 0, &r);
  size_t len = r.count * r.width;
  uint32_t *data = r.data;
  if (data == NULL || len == 0) {
    free(data);
    data = malloc(sizeof(uint32_t));
    len = 0;
  }
  ba = caml_ba_alloc_dims(CAML_BA_INT32 | CAML_BA_C_LAYOUT | CAML_BA_MANAGED,
                          1, data, (intnat) len);
  res = caml_alloc_tuple(2);
  Store_field(res, 0, Val_long(r.width));
  Store_field(res, 1, ba);
  CAMLreturn(res);
}

/* export: the nodes of several diagrams, shared, numbered bottom up
   (0 the empty set, 1 the set of the empty vector, then 2, 3, ... with
   every node after its down and right children), as a flat array of
   (value, down, right) triples, and each root's number. A map from
   Sylvan's node to its number, by open addressing. */

struct nmap { uint64_t *keys; int64_t *vals; size_t cap, n; };

static int64_t nmap_get(struct nmap *m, uint64_t k)
{
  size_t i = (size_t) (k * 0x9E3779B97F4A7C15ULL) & (m->cap - 1);
  while (m->keys[i] != 0) {
    if (m->keys[i] == k) return m->vals[i];
    i = (i + 1) & (m->cap - 1);
  }
  return -1;
}

static void nmap_put(struct nmap *m, uint64_t k, int64_t v)
{
  if (2 * (m->n + 1) > m->cap) {
    struct nmap g = { calloc(2 * m->cap, sizeof(uint64_t)), malloc(2 * m->cap * sizeof(int64_t)), 2 * m->cap, 0 };
    for (size_t j = 0; j < m->cap; j++)
      if (m->keys[j] != 0) nmap_put(&g, m->keys[j], m->vals[j]);
    free(m->keys); free(m->vals); *m = g;
  }
  size_t i = (size_t) (k * 0x9E3779B97F4A7C15ULL) & (m->cap - 1);
  while (m->keys[i] != 0 && m->keys[i] != k) i = (i + 1) & (m->cap - 1);
  if (m->keys[i] == 0) m->n++;
  m->keys[i] = k; m->vals[i] = v;
}

static int64_t num_of(struct nmap *m, MDD x)
{
  if (x == lddmc_false) return 0;
  if (x == lddmc_true) return 1;
  return nmap_get(m, (uint64_t) x);
}

value ml_ldd_export(value roots)
{
  CAMLparam1(roots);
  CAMLlocal3(res, flat, rs);
  size_t nroots = Wosize_val(roots);
  struct nmap m = { calloc(1024, sizeof(uint64_t)), malloc(1024 * sizeof(int64_t)), 1024, 0 };
  size_t cap = 1024, n = 0, scap = 1024, sp = 0;
  int64_t *out = malloc(cap * 3 * sizeof(int64_t));
  MDD *stack = malloc(scap * sizeof(MDD));
  int64_t next = 2;
  for (size_t r = 0; r < nroots; r++) {
    MDD root = Mdd_val(Field(roots, r));
    if (num_of(&m, root) >= 0) continue;
    stack[sp++] = root;
    while (sp > 0) {
      MDD x = stack[sp - 1];
      if (num_of(&m, x) >= 0) { sp--; continue; }
      MDD d = lddmc_getdown(x), rt = lddmc_getright(x);
      int64_t nd = num_of(&m, d), nr = num_of(&m, rt);
      if (nd >= 0 && nr >= 0) {
        sp--;
        if (n == cap) { cap *= 2; out = realloc(out, cap * 3 * sizeof(int64_t)); }
        out[3 * n] = lddmc_getvalue(x); out[3 * n + 1] = nd; out[3 * n + 2] = nr;
        n++;
        nmap_put(&m, (uint64_t) x, next++);
      } else {
        if (sp + 2 > scap) { scap *= 2; stack = realloc(stack, scap * sizeof(MDD)); }
        if (nd < 0) stack[sp++] = d;
        if (nr < 0) stack[sp++] = rt;
      }
    }
  }
  flat = caml_alloc(3 * n, 0);
  for (size_t i = 0; i < 3 * n; i++) Store_field(flat, i, Val_long(out[i]));
  rs = caml_alloc(nroots, 0);
  for (size_t r = 0; r < nroots; r++)
    Store_field(rs, r, Val_long(num_of(&m, Mdd_val(Field(roots, r)))));
  free(out); free(stack); free(m.keys); free(m.vals);
  res = caml_alloc_tuple(2);
  Store_field(res, 0, flat);
  Store_field(res, 1, rs);
  CAMLreturn(res);
}

value ml_ldd_join(value a, value b, value pa, value pb)
{
  CAMLparam4(a, b, pa, pb);
  CAMLreturn(wrap(lddmc_join(Mdd_val(a), Mdd_val(b), Mdd_val(pa), Mdd_val(pb))));
}

/* the node table's use: (nodes in it, live or not yet collected; its
   maximum size) */
value ml_ldd_table_usage(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(r);
  size_t filled, total;
  sylvan_table_usage(&filled, &total);
  r = caml_alloc_tuple(2);
  Store_field(r, 0, Val_long(filled));
  Store_field(r, 1, Val_long(llmsset_get_max_size(nodes)));
  CAMLreturn(r);
}

/* collect the nodes no live value refers to */
value ml_ldd_gc(value unit)
{
  (void) unit;
  sylvan_gc();
  return Val_unit;
}

/* The proof log (doc/certificate.md, A): Sylvan's table collected
   (which also clears the operation cache, so every result is computed
   again, and so logged), collection turned off so that node indices stay
   the names the log uses, then the log opened (vendor/sylvan, branch
   proof-log, [lddmc_log_open]). */
/* the node table and the operation cache at their maximum sizes (the
   limits [ml_ldd_init] set): Sylvan resizes them only when it collects,
   and nothing collects while the log is open, so the logged run needs
   the room first. As the main hook of one collection. The table is mapped
   lazily, so only the nodes made cost memory. */
VOID_TASK_0(ldd_grow_tables)
{
  llmsset_set_size(nodes, llmsset_get_max_size(nodes));
  cache_setsize(cache_getmaxsize());
}

/* the log opened, at [path] or (NULL) in memory */
static int ldd_log_open_at(const char *path)
{
  size_t before = llmsset_get_size(nodes);
  sylvan_gc_hook_main(ldd_grow_tables_CALL);
  /* the collection that calls the main hook ([sylvan_gc] shrinks to fit
     instead) */
  RUN(sylvan_gc_collect);
  if (getenv("MEBI_CERT_STATS") != NULL)
    fprintf(stderr, "[proof log] node table %zu -> %zu buckets (max %zu)\n",
            before, llmsset_get_size(nodes), llmsset_get_max_size(nodes));
  /* back to the configuration's heuristic (SYLVAN_AGGRESSIVE_RESIZE) */
  sylvan_gc_hook_main(sylvan_gc_aggressive_resize_CALL);
  sylvan_clear_cache();
  sylvan_gc_disable();
  int ok = lddmc_log_open(path);
  if (!ok) sylvan_gc_enable();
  return ok;
}

value ml_ldd_log_open(value prefix)
{
  CAMLparam1(prefix);
  CAMLreturn(Val_bool(ldd_log_open_at(String_val(prefix))));
}

/* the log in memory (vendor/sylvan, [lddmc_log_take]) */
value ml_ldd_log_open_mem(value unit)
{
  (void) unit;
  return Val_bool(ldd_log_open_at(NULL));
}

/* closed (the reachable nodes written), collection turned back on; the
   number of records */
/* a top-level call: code is the role times 16 plus the operation's;
   args the arguments and the result (five values) */
value ml_ldd_log_top(value code, value args)
{
  CAMLparam2(code, args);
  if (Wosize_val(args) != 5) caml_invalid_argument("Ldd.log_top");
  long c = Long_val(code);
  lddmc_log_top((int) (c >> 4), (int) (c & 15), Mdd_val(Field(args, 0)),
                Mdd_val(Field(args, 1)), Mdd_val(Field(args, 2)),
                Mdd_val(Field(args, 3)), Mdd_val(Field(args, 4)));
  CAMLreturn(Val_unit);
}

value ml_ldd_log_close(value unit)
{
  (void) unit;
  uint64_t n = lddmc_log_close();
  sylvan_gc_enable();
  return Val_long((long) n);
}

/* a log in memory closed: collection stays off, so that its nodes stay
   the table's until [ml_ldd_log_free] */
value ml_ldd_log_close_mem(value unit)
{
  (void) unit;
  return Val_long((long) lddmc_log_close());
}

/* up to [length buf / 48] records of a log in memory into [buf]: how many */
value ml_ldd_log_take(value buf)
{
  return Val_long((long) lddmc_log_take(Bytes_val(buf), caml_string_length(buf) / 48));
}

/* a log in memory freed, collection turned back on */
value ml_ldd_log_free(value unit)
{
  (void) unit;
  lddmc_log_free();
  sylvan_gc_enable();
  return Val_unit;
}

/* a node of the table by its index: value, down, right, copy */
value ml_ldd_node(value idx)
{
  CAMLparam1(idx);
  CAMLlocal1(r);
  MDD m = (MDD) Long_val(idx);
  r = caml_alloc_tuple(4);
  Store_field(r, 0, Val_long((long) lddmc_getvalue(m)));
  Store_field(r, 1, Val_long((long) lddmc_getdown(m)));
  Store_field(r, 2, Val_long((long) lddmc_getright(m)));
  Store_field(r, 3, Val_bool(lddmc_iscopy(m)));
  CAMLreturn(r);
}
