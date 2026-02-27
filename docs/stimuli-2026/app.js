/* Dashboard logic: load CSV, apply filters, sort, paginate, render table */
const DATA_URL = "./data.csv";
const COLORS_URL = "./colors.csv";

const NUM_COLS = new Set([
  "x1_us", "x2_us", "x3_us", "x4_us", "dX_us", "p_gt0_us", "dS_us",
  "x1_mg", "x2_mg", "x3_mg", "x4_mg", "dX_mg", "p_gt0_mg", "dS_mg", "dS_diff"
]);

let raw = [];
let view = [];
let page = 0;
let tableCols = [];
let colorHex = new Map(); // code -> "#rrggbb"

const el = (id) => document.getElementById(id);

function fmt(x) {
  if (x == null || Number.isNaN(x)) return "";
  if (typeof x === "number") return x.toFixed(4);
  return String(x);
}

function canonizePair(d) {
  const a = d.conA;
  const b = d.conB;
  if (a == null || b == null) return { conA: a, conB: b };
  if (String(a).localeCompare(String(b)) <= 0) return { conA: a, conB: b };
  return { conA: b, conB: a };
}

function getFilterState() {
  const filters = [];
  document.querySelectorAll(".filter").forEach(box => {
    const col = box.dataset.col;
    const enabled = box.querySelector(".fEnable").checked;
    const op = box.querySelector(".fOp").value;
    const thr = +box.querySelector(".fSlider").value;
    if (enabled) filters.push({ col, op, thr });
  });
  const search = el("search").value.trim().toLowerCase();
  const canon = el("canonPair").checked;
  const sortBy = el("sortBy").value;
  const pageSize = +el("pageSize").value;
  return { filters, search, canon, sortBy, pageSize };
}

function applyFiltersAndSort() {
  const { filters, search, canon, sortBy } = getFilterState();
  const searchTokens = search.length ? search.split(/\s+/).filter(Boolean) : [];

  view = raw.filter(d => {
    if (searchTokens.length) {
      const a = String(d.conA ?? "").toLowerCase();
      const b = String(d.conB ?? "").toLowerCase();
      const ok = searchTokens.every(t => (a.includes(t) || b.includes(t)));
      if (!ok) return false;
    }
    for (const f of filters) {
      let v = d[f.col];
      if (v == null || Number.isNaN(v)) return false;

      // use absolute value for dS_diff filter
      if (f.col === "dS_diff") {
        v = Math.abs(v);
      }

      if (f.op === ">=" && !(v >= f.thr)) return false;
      if (f.op === "<=" && !(v <= f.thr)) return false;
    }

    return true;
  });

  if (canon) {
    view = view.map(d => {
      const p = canonizePair(d);
      return { ...d, conA: p.conA, conB: p.conB };
    });
  }

  const cmpStr = (x, y) => String(x ?? "").localeCompare(String(y ?? ""));
  const cmpNum = (x, y) => (+x) - (+y);

  view.sort((a, b) => {
    if (sortBy === "conA") {
      const c1 = cmpStr(a.conA, b.conA);
      return c1 !== 0 ? c1 : cmpStr(a.conB, b.conB);
    }
    if (sortBy === "conB") {
      const c1 = cmpStr(a.conB, b.conB);
      return c1 !== 0 ? c1 : cmpStr(a.conA, b.conA);
    }

    if (sortBy === "withinPair_dSdiff_desc" || sortBy === "withinPair_dSdiff_asc") {
      const cA = cmpStr(a.conA, b.conA);
      if (cA !== 0) return cA;
      const cB = cmpStr(a.conB, b.conB);
      if (cB !== 0) return cB;

      const av = Math.abs(a.dS_diff);
      const bv = Math.abs(b.dS_diff);
      const c = cmpNum(av, bv);
      return (sortBy === "withinPair_dSdiff_desc") ? -c : c;
    }

    const parts = sortBy.split("_");
    const col = parts[0] + "_" + parts[1]; // e.g. dS_us
    const dir = parts[2]; // asc/desc
    const c = cmpNum(a[col], b[col]);
    return dir === "desc" ? -c : c;
  });

  page = 0;
  render();
}

function render() {
  const { pageSize } = getFilterState();
  const total = view.length;
  const totalPages = Math.max(1, Math.ceil(total / pageSize));
  page = Math.min(page, totalPages - 1);

  el("resultCount").textContent = `${total.toLocaleString()} rows match filters`;
  el("pageInfo").textContent = `page ${page + 1} / ${totalPages}`;

  el("prev").disabled = (page <= 0);
  el("next").disabled = (page >= totalPages - 1);

  const start = page * pageSize;
  const slice = view.slice(start, start + pageSize);

  const theadRow = document.getElementById("theadRow");
  theadRow.innerHTML = "";
  tableCols.forEach(col => {
    const th = document.createElement("th");
    th.textContent = col;
    th.classList.add("col-" + col);

    theadRow.appendChild(th);
  });

  const tbody = document.getElementById("tbody");
  tbody.innerHTML = "";
  let prevPair = null;

  for (const d of slice) {
    const tr = document.createElement("tr");

    const pair = `${d.conA}|||${d.conB}`;
    if (prevPair !== null && pair !== prevPair) {
      tr.classList.add("pairSeparator");
    }
    prevPair = pair;

    for (const col of tableCols) {
      const td = document.createElement("td");
      td.classList.add("col-" + col);

      const v = d[col];

      if (col === "col1" || col === "col2") {
        const code = v ?? "";
        td.textContent = code;
        // const hex = colorHex.get(String(code).trim());
        const hex = colorHex.get(code);

        if (hex) {
          // color entire cell
          td.style.background = hex;

          // auto text color for contrast
          const h = hex.replace("#", "");
          if (h.length === 6) {
            const r = parseInt(h.slice(0, 2), 16);
            const g = parseInt(h.slice(2, 4), 16);
            const b = parseInt(h.slice(4, 6), 16);
            const yiq = (r * 299 + g * 587 + b * 114) / 1000;
            td.style.color = (yiq >= 140) ? "#111" : "#fff";
          }
        }
      } else if (typeof v === "number") {
        td.textContent = fmt(v);
        td.classList.add("mono");
      } else {
        td.textContent = (v ?? "");
      }

      tr.appendChild(td);
    }
    tbody.appendChild(tr);
  }
}

function initFilterControls() {
  const cols = ["dS_us", "dS_mg", "dS_diff", "dX_us", "dX_mg"];

  const stats = {};
  for (const c of cols) {
    let mn = Infinity, mx = -Infinity;
    for (const d of raw) {
      let v = d[c];

      if (v == null || Number.isNaN(v)) continue;

      if (c === "dS_diff") v = Math.abs(v);

      if (v < mn) mn = v;
      if (v > mx) mx = v;
    }

    stats[c] = { min: mn, max: mx };
  }

  document.querySelectorAll(".filter").forEach(box => {
    const col = box.dataset.col;
    const { min, max } = stats[col];
    const slider = box.querySelector(".fSlider");
    const minEl = box.querySelector(".fMin");
    const maxEl = box.querySelector(".fMax");
    const valEl = box.querySelector(".fValue");

    slider.min = min;
    slider.max = max;
    slider.value = (min + max) / 2;

    minEl.textContent = fmt(min);
    maxEl.textContent = fmt(max);
    valEl.textContent = fmt(+slider.value);

    slider.addEventListener("input", () => {
      valEl.textContent = fmt(+slider.value);
      applyFiltersAndSort();
    });
    box.querySelector(".fEnable").addEventListener("change", applyFiltersAndSort);
    box.querySelector(".fOp").addEventListener("change", applyFiltersAndSort);
  });
}

function bindUI() {
  el("search").addEventListener("input", () => {
    clearTimeout(window.__t);
    window.__t = setTimeout(applyFiltersAndSort, 150);
  });
  el("canonPair").addEventListener("change", applyFiltersAndSort);
  el("sortBy").addEventListener("change", applyFiltersAndSort);
  el("pageSize").addEventListener("change", () => { page = 0; render(); });

  el("prev").addEventListener("click", () => { page -= 1; render(); });
  el("next").addEventListener("click", () => { page += 1; render(); });

  el("reset").addEventListener("click", () => {
    el("search").value = "";
    el("canonPair").checked = true;
    el("sortBy").value = "conA";
    document.querySelectorAll(".filter").forEach(box => {
      box.querySelector(".fEnable").checked = false;
      box.querySelector(".fOp").value = ">=";
      const slider = box.querySelector(".fSlider");
      slider.value = (+slider.min + +slider.max) / 2;
      box.querySelector(".fValue").textContent = fmt(+slider.value);
    });
    applyFiltersAndSort();
  });
  el("exportXlsx").addEventListener("click", exportFilteredToXlsx);

}

async function loadColors() {
  try {
    const rows = await d3.csv(COLORS_URL);

    rows.forEach(r => {
      const code = r.code?.trim();
      const hex = r.hex?.trim();

      if (code && hex) {
        colorHex.set(code, hex);
      }
    });

    return { ok: true, n: colorHex.size };

  } catch (e) {
    console.warn("Could not load colors.csv", e);
    return { ok: false, n: 0 };
  }
}

function exportFilteredToXlsx() {
  // Export the full filtered/sorted view (NOT just the current page)
  const rows = view.map(d => {
    const out = {};
    for (const c of tableCols) out[c] = d[c];
    return out;
  });

  const ws = XLSX.utils.json_to_sheet(rows, { header: tableCols });
  const wb = XLSX.utils.book_new();
  XLSX.utils.book_append_sheet(wb, ws, "filtered_view");

  // Optional: freeze header row
  ws["!freeze"] = { xSplit: 0, ySplit: 1 };

  const name = `filtered_view_${new Date().toISOString().slice(0, 19).replace(/[:T]/g, "-")}.xlsx`;
  XLSX.writeFile(wb, name);
}


async function main() {
  console.log("Loading CSV…");
  const t0 = performance.now();

  const colorInfo = await loadColors();

  raw = await d3.csv(DATA_URL, (row) => {
    const out = {};
    for (const [k, v] of Object.entries(row)) {
      if (NUM_COLS.has(k)) {
        const n = +v;
        out[k] = Number.isFinite(n) ? n : null;
      } else {
        out[k] = v;
      }
    }
    return out;
  });

  // IMPORTANT: column order matches the CSV header order
  tableCols = raw.columns ? [...raw.columns] : Object.keys(raw[0] ?? {});

  const t1 = performance.now();
  const msg = `Loaded ${raw.length.toLocaleString()} rows in ${(t1 - t0).toFixed(0)} ms`
    + (colorInfo.ok ? ` • color patches: ${colorInfo.n} mapped` : " • color patches: (colors.csv not loaded)");
  el("stats").textContent = msg;

  initFilterControls();
  bindUI();
  applyFiltersAndSort();
}

main().catch(err => {
  console.error(err);
  el("stats").textContent = "Failed to load CSV (see console).";
});
