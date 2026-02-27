/* global d3 */

const DATASETS = {
    res_all: "data/res_all.csv",
    res_extreme: "data/res_extreme.csv",
    res_topK: "data/res_topK.csv",
    res_random: "data/res_random.csv",
};

const COLORBOOK_URL = "data/bcp37.csv";
let COLORBOOK = new Map(); // key: code -> { R,G,B,hex,name }
let COLORBOOK_BY_NAME = new Map(); // key: lower(name) -> same object


// Quadrant mapping + positions
const QUADS = [
    { key: "MG_low__US_high", title: "MG low / US high", pos: "tl" },
    { key: "MG_high__US_high", title: "MG high / US high", pos: "tr" },
    { key: "MG_low__US_low", title: "MG low / US low", pos: "bl" },
    { key: "MG_high__US_low", title: "MG high / US low", pos: "br" },
];

const els = {
    datasetSelect: document.getElementById("datasetSelect"),
    pairSearch: document.getElementById("pairSearch"),
    selectAllBtn: document.getElementById("selectAllBtn"),
    clearBtn: document.getElementById("clearBtn"),
    pairList: d3.select("#pairList"),
    pairMeta: d3.select("#pairMeta"),
    status: d3.select("#status"),
    viz: d3.select("#viz"),
    tooltip: d3.select("#tooltip"),
};

let state = {
    datasetKey: "res_all",
    rows: [],
    pairs: [],              // [{key, a, b, count}]
    selectedPairKeys: new Set(),
    search: "",
};

// ---------- Helpers (robust column picking) ----------
function pick(row, candidates) {
    for (const c of candidates) {
        if (row[c] !== undefined) return row[c];
    }
    return undefined;
}
function getConceptA(r) { return pick(r, ["concept_a", "conceptA", "concept_1", "concept1", "a"]); }
function getConceptB(r) { return pick(r, ["concept_b", "conceptB", "concept_2", "concept2", "b"]); }
function getCategory(r) { return pick(r, ["category", "cat", "quadrant"]); }

function getColorA(r) { return pick(r, ["color_a", "colorA", "color_1", "color1"]); }
function getColorB(r) { return pick(r, ["color_b", "colorB", "color_2", "color2"]); }

function getDeltaMg(r) {
    const v = pick(r, ["deltaS_mg", "delta_mg", "mg_deltaS"]);
    return v === undefined || v === "" ? null : +v;
}
function getDeltaUs(r) {
    const v = pick(r, ["deltaS_us", "delta_us", "us_deltaS"]);
    return v === undefined || v === "" ? null : +v;
}
function getExtremeness(r) {
    const v = pick(r, ["extremeness", "extreme", "score_extreme"]);
    return v === undefined || v === "" ? null : +v;
}

// Optional RGB swatches if present
function getRgb(r, which /* 'a'|'b' */) {
    // 1) If RGB columns exist in the row, use them first
    const R = pick(r, which === "a"
        ? ["color_a_R", "R_a", "r_a", "R1", "r1"]
        : ["color_b_R", "R_b", "r_b", "R2", "r2"]
    );
    const G = pick(r, which === "a"
        ? ["color_a_G", "G_a", "g_a", "G1", "g1"]
        : ["color_b_G", "G_b", "g_b", "G2", "g2"]
    );
    const B = pick(r, which === "a"
        ? ["color_a_B", "B_a", "b_a", "B1", "b1"]
        : ["color_b_B", "B_b", "b_b", "B2", "b2"]
    );

    if (![R, G, B].some(v => v === undefined || v === "")) {
        const rr = Math.max(0, Math.min(255, +R));
        const gg = Math.max(0, Math.min(255, +G));
        const bb = Math.max(0, Math.min(255, +B));
        return `rgb(${rr},${gg},${bb})`;
    }

    // 2) Otherwise: lookup by color code (preferred) or name in bcp37
    const codeOrName = (which === "a" ? getColorA(r) : getColorB(r));
    if (!codeOrName) return null;

    const key = String(codeOrName).trim();
    const byCode = COLORBOOK.get(key);
    if (byCode) return `rgb(${byCode.R},${byCode.G},${byCode.B})`;

    const byName = COLORBOOK_BY_NAME.get(key.toLowerCase());
    if (byName) return `rgb(${byName.R},${byName.G},${byName.B})`;

    return null;
}


function pairKey(a, b) { return `${a} × ${b}`; }

function groupBy(arr, keyFn) {
    const m = new Map();
    arr.forEach(x => {
        const k = keyFn(x);
        if (!m.has(k)) m.set(k, []);
        m.get(k).push(x);
    });
    return m;
}

// ---------- Loading ----------
async function loadColorbook() {
    const cb = await d3.csv(COLORBOOK_URL);

    COLORBOOK.clear();
    COLORBOOK_BY_NAME.clear();

    cb.forEach(r => {
        const code = (r.code || "").trim();
        if (!code) return;

        const entry = {
            code,
            name: r.name,
            R: +r.R,
            G: +r.G,
            B: +r.B,
            hex: r.hex
        };

        COLORBOOK.set(code, entry);
        if (r.name) COLORBOOK_BY_NAME.set(String(r.name).toLowerCase().trim(), entry);
    });
}

async function loadDataset(datasetKey) {
    state.datasetKey = datasetKey;
    state.selectedPairKeys = new Set();
    els.status.text("Loading CSV…");

    const url = DATASETS[datasetKey];
    const rows = await d3.csv(url);

    state.rows = rows;

    // build concept pairs list
    const counts = new Map();
    rows.forEach(r => {
        const a = getConceptA(r);
        const b = getConceptB(r);
        if (!a || !b) return;
        const k = pairKey(a, b);
        counts.set(k, (counts.get(k) || 0) + 1);
    });

    state.pairs = Array.from(counts.entries())
        .map(([k, count]) => {
            const [a, b] = k.split(" × ");
            return { key: k, a, b, count };
        })
        .sort((p1, p2) => p1.key.localeCompare(p2.key));

    els.status.text(`Loaded ${rows.length.toLocaleString()} rows from ${datasetKey}. Select concept pairs to render quadrants.`);
    renderPairList();
    renderViz();
}

// ---------- UI: Pair list ----------
function getFilteredPairs() {
    const q = state.search.trim().toLowerCase();
    if (!q) return state.pairs;
    return state.pairs.filter(p => p.key.toLowerCase().includes(q));
}

function renderPairList() {
    const filtered = getFilteredPairs();
    els.pairMeta.text(`${filtered.length} shown / ${state.pairs.length} total`);

    const items = els.pairList
        .selectAll(".pairItem")
        .data(filtered, d => d.key);

    const itemsEnter = items.enter()
        .append("label")
        .attr("class", "pairItem");

    itemsEnter.append("input")
        .attr("type", "checkbox")
        .on("change", (event, d) => {
            if (event.target.checked) state.selectedPairKeys.add(d.key);
            else state.selectedPairKeys.delete(d.key);
            renderViz();
        });

    itemsEnter.append("div").attr("class", "pairName");
    itemsEnter.append("div").attr("class", "pairCount");

    // update
    items.merge(itemsEnter).select("input")
        .property("checked", d => state.selectedPairKeys.has(d.key));

    items.merge(itemsEnter).select(".pairName")
        .text(d => d.key);

    items.merge(itemsEnter).select(".pairCount")
        .text(d => d.count);

    items.exit().remove();
}

// ---------- Viz rendering (D3 DOM) ----------
function renderViz() {
    const selected = Array.from(state.selectedPairKeys);
    els.viz.selectAll("*").remove();

    if (selected.length === 0) {
        els.viz.append("div")
            .attr("class", "empty")
            .text("No concept pairs selected. Use the left panel to select one or more pairs.");
        return;
    }

    // subset rows for selected pairs
    const subset = state.rows.filter(r => {
        const a = getConceptA(r);
        const b = getConceptB(r);
        if (!a || !b) return false;
        return state.selectedPairKeys.has(pairKey(a, b));
    });

    const byPair = groupBy(subset, r => pairKey(getConceptA(r), getConceptB(r)));

    const pairSections = els.viz.selectAll(".pairSection")
        .data(selected, d => d)
        .enter()
        .append("div")
        .attr("class", "pairSection");

    // header
    const headers = pairSections.append("div").attr("class", "pairHeader");
    headers.append("div").attr("class", "h1").text(d => d);
    headers.append("div").attr("class", "h2")
        .text(d => {
            const n = (byPair.get(d) || []).length;
            return `${n} row${n === 1 ? "" : "s"} in dataset`;
        });

    // quadrants grid per pair
    const quadGrid = pairSections.append("div").attr("class", "quadrants");

    // For each quadrant, we append in *grid order*:
    // row1: TL, TR ; row2: BL, BR
    const gridOrder = ["tl", "tr", "bl", "br"];
    const quadDefsInGridOrder = gridOrder.map(pos => QUADS.find(q => q.pos === pos));

    quadGrid.selectAll(".quad")
        .data(d => {
            const rows = byPair.get(d) || [];
            const byCat = groupBy(rows, r => getCategory(r));
            return quadDefsInGridOrder.map(q => ({
                pairKey: d,
                quadKey: q.key,
                title: q.title,
                rows: (byCat.get(q.key) || [])
            }));
        })
        .enter()
        .append("div")
        .attr("class", "quad")
        .each(function (qd) {
            const quad = d3.select(this);

            quad.append("div").attr("class", "quadTitle").text(qd.title);
            quad.append("div").attr("class", "quadSub").text(qd.quadKey);

            const body = quad.append("div").attr("class", "items");

            if (qd.rows.length === 0) {
                body.append("div").attr("class", "empty").text("No color pairs.");
                return;
            }

            const item = body.selectAll(".item")
                .data(qd.rows)
                .enter()
                .append("div")
                .attr("class", "item")
                .on("mousemove", (event, r) => showTooltip(event, r))
                .on("mouseleave", hideTooltip);

            // swatches
            const sw = item.append("div").attr("class", "swatches");
            sw.append("div")
                .attr("class", "swatch")
                .style("background", r => getRgb(r, "a") || "transparent");
            sw.append("div")
                .attr("class", "swatch")
                .style("background", r => getRgb(r, "b") || "transparent");

            // text
            const text = item.append("div");
            text.append("div")
                .attr("class", "itemMain")
                .text(r => `${getColorA(r)} – ${getColorB(r)}`);

            text.append("div")
                .attr("class", "itemSub")
                .html(r => {
                    const ex = getExtremeness(r);
                    const mg = getDeltaMg(r);
                    const us = getDeltaUs(r);
                    const parts = [];
                    if (ex !== null) parts.push(`ext: ${ex.toFixed(2)}`);
                    // if (mg !== null) parts.push(`mg: ${mg.toFixed(2)}`);
                    // if (us !== null) parts.push(`us: ${us.toFixed(2)}`);
                    return parts.join("&nbsp;&nbsp;•&nbsp;&nbsp;");
                });
        });
}

// ---------- Tooltip ----------
function showTooltip(event, r) {
    const a = getConceptA(r), b = getConceptB(r);
    const ca = getColorA(r), cb = getColorB(r);
    const cat = getCategory(r);
    const mg = getDeltaMg(r), us = getDeltaUs(r), ex = getExtremeness(r);

    const lines = [
        `<div style="font-weight:700;margin-bottom:6px;">${a} × ${b}</div>`,
        `<div><b>category</b>: ${cat}</div>`,
        `<div><b>colors</b>: ${ca} – ${cb}</div>`,
    ];
    if (mg !== null) lines.push(`<div><b>deltaS_mg</b>: ${mg}</div>`);
    if (us !== null) lines.push(`<div><b>deltaS_us</b>: ${us}</div>`);
    if (ex !== null) lines.push(`<div><b>extremeness</b>: ${ex}</div>`);

    els.tooltip
        .style("opacity", 1)
        .html(lines.join(""))
        .style("left", (event.clientX + 14) + "px")
        .style("top", (event.clientY + 14) + "px");
}
function hideTooltip() {
    els.tooltip.style("opacity", 0);
}

// ---------- Wire controls ----------
els.datasetSelect.addEventListener("change", (e) => {
    loadDataset(e.target.value);
});

els.pairSearch.addEventListener("input", (e) => {
    state.search = e.target.value || "";
    renderPairList();
});

els.selectAllBtn.addEventListener("click", () => {
    const filtered = getFilteredPairs();
    filtered.forEach(p => state.selectedPairKeys.add(p.key));
    renderPairList();
    renderViz();
});

els.clearBtn.addEventListener("click", () => {
    state.selectedPairKeys = new Set();
    renderPairList();
    renderViz();
});

// ---------- Start ----------
(async function start() {
    els.status.text("Loading colorbook…");
    await loadColorbook();
    await loadDataset(state.datasetKey);
})();
