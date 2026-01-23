/* global d3 */

import {
    els, normalizePair, parseRow, getSelectedPairKeys, setSelectedPairKeys, getSubsetForPair, computeColorOrder, drawHist, tooltipHTMLForCell, tooltipHTMLForDiff
} from "./utils.js";

const FILES = {
    mg: "data/mg_pairwise_sem_dis_alpha_mg_vis.csv",
    us: "data/us_pairwise_sem_dis_alpha_us_vis.csv",
    bcp: "data/bcp37.csv"
};

let COLOR_LOOKUP = new Map(); // code -> {L,a,b,c,h,R,G,B}

const state = {
    group: "both",
    view: "heatmap",
    selectedPairs: [],        // <-- MULTI
    order: "alpha",
    cols: 5,
    half: "full", // "full" | "top"
};

// ---------- helpers ----------

function setTooltip(html, x, y) {
    els.tooltip
        .style("opacity", 1)
        .html(html)
        .style("left", `${x + 1}px`)
        .style("top", `${y + 1}px`);
}

function hideTooltip() {
    els.tooltip.style("opacity", 0);
}

function cssForColorCode(code) {
    const d = COLOR_LOOKUP.get(code);
    if (!d || !d.hex) return "#fff";  // fallback
    // return d.hex.startsWith("#") ? d.hex : `#${d.hex}`;
    return d.hex;
}

function labcLine(code) {
    const d = COLOR_LOOKUP.get(code);
    if (!d) return `${code}: (no Labc found)`;
    return `${code}: L=${d.L.toFixed(1)} a=${d.a.toFixed(1)} b=${d.b.toFixed(1)} c=${d.c.toFixed(1)}`;
    // return `${code}: L=${d.L.toFixed(1)} a=${d.a.toFixed(1)} b=${d.b.toFixed(1)} c=${d.c.toFixed(1)}`;
}

// key -> { tipEl: HTMLElement, cellEl: SVGRectElement }
const PINNED = new Map();

function makePinnedTooltip(key, html, x, y) {
    const div = document.createElement("div");
    div.className = "tooltip pinned";
    div.dataset.key = key;
    div.style.opacity = 1;
    div.style.left = `${x + 12}px`;
    div.style.top = `${y + 12}px`;
    div.innerHTML = html;

    // Clicking the pinned tooltip closes it (and will also remove highlight)
    div.addEventListener("click", (e) => {
        e.stopPropagation();
        removePinnedTooltip(key);
    });

    document.body.appendChild(div);
    return div;
}

function removePinnedTooltip(key) {
    const entry = PINNED.get(key);
    if (!entry) return;

    // remove tooltip
    entry.tipEl?.remove();

    // remove highlight if the cell is still in DOM
    if (entry.cellEl) {
        d3.select(entry.cellEl).classed("cell-selected", false);
    }

    PINNED.delete(key);
}

function togglePinnedTooltip(key, html, x, y, cellEl) {
    if (PINNED.has(key)) {
        removePinnedTooltip(key);
    } else {
        const tipEl = makePinnedTooltip(key, html, x, y);
        PINNED.set(key, { tipEl, cellEl });

        // add highlight
        if (cellEl) d3.select(cellEl).classed("cell-selected", true);
    }
}

// ---------- load data ----------
Promise.all([
    d3.csv(FILES.mg, (r) => parseRow(r, "mg")),
    d3.csv(FILES.us, (r) => parseRow(r, "us")),
    d3.csv(FILES.bcp, (r) => ({
        code: r.code,
        name: r.name,
        hex: r.hex,
        L: +r.L, a: +r.a, b: +r.b, c: +r.c, h: +r.h,
        R: +r.R, G: +r.G, B: +r.B
    })),
]).then(([mg, us, bcp]) => {
    // build lookup by color code used in your semantic-distance datasets
    COLOR_LOOKUP = new Map(bcp.map(d => [d.code, d]));
    const all = mg.concat(us);

    // all unique concept pairs
    const pairKeys = Array.from(
        d3.rollup(all, v => v.length, d => normalizePair(d.concept_a, d.concept_b)).keys()
    ).sort((p1, p2) => {
        const [a1, b1] = p1.split("|||");
        const [a2, b2] = p2.split("|||");
        return (a1 + b1).localeCompare(a2 + b2);
    });

    // populate multi-select
    els.pairSelect.innerHTML = "";
    for (const key of pairKeys) {
        const [a, b] = key.split("|||");
        const opt = document.createElement("option");
        opt.value = key;
        opt.textContent = `${a} — ${b}`;
        els.pairSelect.appendChild(opt);
    }

    els.halfSelect.addEventListener("change", () => {
        state.half = els.halfSelect.value; // "full" or "top"
        render(all);
    });

    // default: first pair selected
    state.selectedPairs = [pairKeys[0]];
    setSelectedPairKeys(state.selectedPairs);

    // wire controls
    els.groupSelect.addEventListener("change", () => {
        state.group = els.groupSelect.value;
        render(all);
    });

    els.viewSelect.addEventListener("change", () => {
        state.view = els.viewSelect.value;
        render(all);
    });

    els.orderSelect.addEventListener("change", () => {
        state.order = els.orderSelect.value;
        render(all);
    });

    els.pairSelect.addEventListener("change", () => {
        // if user manually changes selection, uncheck "select all" unless it matches all
        state.selectedPairs = getSelectedPairKeys();
        els.allPairs.checked = (state.selectedPairs.length === els.pairSelect.options.length);
        render(all);
    });

    els.colsInput.addEventListener("input", () => {
        const v = +els.colsInput.value;
        state.cols = Number.isFinite(v) ? Math.max(1, Math.min(12, v)) : 5;
        els.colsInput.value = state.cols;
        render(all);
    });

    els.allPairs.addEventListener("change", () => {
        if (els.allPairs.checked) {
            // select all
            const allKeys = Array.from(els.pairSelect.options).map(o => o.value);
            state.selectedPairs = allKeys;
            setSelectedPairKeys(allKeys);
        } else {
            // if unchecked and nothing selected, fall back to first option
            const current = getSelectedPairKeys();
            if (current.length === 0 && els.pairSelect.options.length > 0) {
                const first = els.pairSelect.options[0].value;
                state.selectedPairs = [first];
                setSelectedPairKeys([first]);
            } else {
                state.selectedPairs = current;
            }
        }
        render(all);
    });

    els.clearPairs.addEventListener("click", () => {
        els.allPairs.checked = false;
        // clear selection, then select first (so we always render something)
        for (const opt of els.pairSelect.options) opt.selected = false;
        if (els.pairSelect.options.length > 0) {
            const first = els.pairSelect.options[0].value;
            state.selectedPairs = [first];
            setSelectedPairKeys([first]);
        } else {
            state.selectedPairs = [];
        }
        render(all);
    });

    // initial render
    render(all);
});

// ---------- heatmap rendering  ----------
function renderHeatmap(container, records, title, compact = false) {
    container.innerHTML = "";
    // mu_D exists only for the stored direction (color_1 < color_2)
    const mu = new Map();
    for (const d of records) {
        mu.set(`${d.color_1}|||${d.color_2}`, d.mu_D);
    }

    // Association strengths exist only for stored direction (color_1 < color_2)
    const assoc = new Map();
    for (const d of records) {
        assoc.set(`${d.color_1}|||${d.color_2}`, {
            A_to_C1: d.A_to_C1,
            A_to_C2: d.A_to_C2,
            B_to_C1: d.B_to_C1,
            B_to_C2: d.B_to_C2,
        });
    }

    const card = document.createElement("div");
    card.className = "card";
    container.appendChild(card);

    const heatmapId = `hm-${Math.random().toString(16).slice(2)}`;

    const width = compact ? 250 : 980;
    const height = compact ? 270 : 840;

    const margin = compact
        ? { top: 28, right: 8, bottom: 8, left: 8 }
        : { top: 70, right: 30, bottom: 30, left: 90 };

    const svg = d3.select(card)
        .append("svg")
        .attr("viewBox", `0 0 ${width} ${height}`);

    svg.append("text")
        .attr("x", margin.left)
        .attr("y", compact ? 18 : 28)
        .attr("font-size", compact ? 12 : 14)
        .attr("font-weight", 600)
        .text(title);

    if (!records || records.length === 0) {
        svg.append("text")
            .attr("x", margin.left)
            .attr("y", compact ? 38 : 55)
            .attr("font-size", 12)
            .text("No data.");
        return;
    }

    const conceptA = records[0]?.concept_a ?? "";
    const conceptB = records[0]?.concept_b ?? "";

    // Using a consistent order of colors on both axes (still fine),
    // the semantics are now explicit: y is color_1 (row), x is color_2 (column).
    const colorOrder = computeColorOrder(records, state);

    const idx = new Map(colorOrder.map((c, i) => [c, i]));

    // Build lookup so we can retrieve distance for (rowColor, colColor)
    // We include symmetric entries so the full matrix is populated.
    const dist = new Map();
    for (const d of records) {
        dist.set(`${d.color_1}|||${d.color_2}`, d.semantic_distance);
        dist.set(`${d.color_2}|||${d.color_1}`, d.semantic_distance);
    }
    // diagonal
    for (const c of colorOrder) dist.set(`${c}|||${c}`, 0);

    const innerW = width - margin.left - margin.right;
    const innerH = height - margin.top - margin.bottom;

    // x = columns = color_2
    // y = rows    = color_1
    const x = d3.scaleBand().domain(colorOrder).range([0, innerW]);
    const y = d3.scaleBand().domain(colorOrder).range([0, innerH]);

    // Values for color scale
    const vals = [];
    for (const row of colorOrder) {
        for (const col of colorOrder) {
            vals.push(dist.get(`${row}|||${col}`));
        }
    }
    const vmin = d3.min(vals);
    const vmax = d3.max(vals);
    const fill = d3.scaleSequential(d3.interpolateViridis).domain([vmin, vmax]);

    const g = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);

    // Axes labels only when not compact
    if (!compact) {
        // Left axis = rows = color_1
        g.append("g")
            .attr("class", "axis")
            .call(d3.axisLeft(y).tickSize(0));

        // Top axis = columns = color_2
        g.append("g")
            .attr("class", "axis")
            .call(d3.axisTop(x).tickSize(0))
            .selectAll("text")
            .attr("text-anchor", "start")
            .attr("transform", "rotate(-90)")
            .attr("dx", "0.6em")
            .attr("dy", "0.6em");
    }

    // Tiles: row = color_1 (y), col = color_2 (x)
    const tiles = [];
    for (const row of colorOrder) {
        for (const col of colorOrder) {
            const storedKey = (row < col) ? `${row}|||${col}` : `${col}|||${row}`;
            const baseAssoc = assoc.get(storedKey);
            const swapped = !(row < col); // if row >= col, we're looking at the mirrored cell

            // Map stored (C1,C2) strengths onto (rowColor,colColor)
            const A_to_row = baseAssoc ? (swapped ? baseAssoc.A_to_C2 : baseAssoc.A_to_C1) : 0;
            const A_to_col = baseAssoc ? (swapped ? baseAssoc.A_to_C1 : baseAssoc.A_to_C2) : 0;
            const B_to_row = baseAssoc ? (swapped ? baseAssoc.B_to_C2 : baseAssoc.B_to_C1) : 0;
            const B_to_col = baseAssoc ? (swapped ? baseAssoc.B_to_C1 : baseAssoc.B_to_C2) : 0;

            tiles.push({
                rowColor: row,   // shown as "color 1" in tooltip
                colColor: col,   // shown as "color 2" in tooltip
                d: dist.get(`${row}|||${col}`),
                rowName: COLOR_LOOKUP.get(row)?.name,
                colName: COLOR_LOOKUP.get(col)?.name,

                // NEW: association strengths mapped to the orientation shown in tooltip
                A_to_row,
                A_to_col,
                B_to_row,
                B_to_col,

                // mu_D / Δx: show only above diagonal; below diagonal => null (NA in tooltip)
                deltaX: (() => {
                    if (row === col) return 0;
                    const key = (row < col) ? `${row}|||${col}` : `${col}|||${row}`; // stored direction
                    const base = mu.get(key);
                    if (!Number.isFinite(base)) return null;

                    const upper = idx.get(row) <= idx.get(col); // above diag (by displayed order)
                    return upper ? base : null;
                })(),
            });

        }
    }

    g.append("g")
        .selectAll("rect")
        .data(tiles)
        .join("rect")
        .attr("x", t => x(t.colColor))   // columns
        .attr("y", t => y(t.rowColor))   // rows
        .attr("width", x.bandwidth())
        .attr("height", y.bandwidth())
        .attr("fill", t => {
            const r = idx.get(t.rowColor);
            const c = idx.get(t.colColor);
            const show = (state.half === "full") || (r <= c);

            if (show) return fill(t.d);

            // faint placeholder for hidden bottom-half
            return "rgba(107, 86, 86, 0.08)";
        })
        .attr("stroke", t => {
            const r = idx.get(t.rowColor);
            const c = idx.get(t.colColor);
            const show = (state.half === "full") || (r <= c);

            // no stroke for real cells; light grid for hidden
            return show ? "none" : "rgba(130, 116, 116, 0.1)";
        })
        .style("pointer-events", t => {
            const r = idx.get(t.rowColor);
            const c = idx.get(t.colColor);
            const show = (state.half === "full") || (r <= c);
            return show ? "all" : "none";
        })
        .on("mousemove", (event, t) => {
            setTooltip(tooltipHTMLForCell(t, records, cssForColorCode), event.clientX, event.clientY);
        })
        .on("mouseleave", () => {
            hideTooltip();
        })
        .on("click", function (event, t) {
            event.stopPropagation();
            const key = `${heatmapId}|||${t.rowColor}|||${t.colColor}`;
            togglePinnedTooltip(key, tooltipHTMLForCell(t, records, cssForColorCode), event.clientX, event.clientY, this);
        });

    // Legend only for non-compact
    if (!compact) {
        const legendW = 240;
        const legendH = 10;
        const legendX = margin.left;
        const legendY = height - 28;

        const legend = svg.append("g").attr("class", "legend");

        legend.append("text")
            .attr("x", legendX)
            .attr("y", legendY - 8)
            .text(`scale: ${vmin.toFixed(2)} → ${vmax.toFixed(2)}`);

        const gradId = `grad-${Math.random().toString(16).slice(2)}`;
        const defs = svg.append("defs");
        const grad = defs.append("linearGradient")
            .attr("id", gradId)
            .attr("x1", "0%").attr("x2", "100%")
            .attr("y1", "0%").attr("y2", "0%");

        const stops = d3.range(0, 1.0001, 0.1);
        grad.selectAll("stop")
            .data(stops)
            .join("stop")
            .attr("offset", d => `${d * 100}%`)
            .attr("stop-color", d => fill(vmin + d * (vmax - vmin)));

        legend.append("rect")
            .attr("x", legendX)
            .attr("y", legendY)
            .attr("width", legendW)
            .attr("height", legendH)
            .attr("fill", `url(#${gradId})`)
            .attr("stroke", "#ccc");
    }
}

// ---------- diff heatmap rendering  ----------
function renderDiffHeatmap(container, mgRecords, usRecords, title, compact = false) {
    container.innerHTML = "";

    const card = document.createElement("div");
    card.className = "card";
    container.appendChild(card);

    const heatmapId = `diff-${Math.random().toString(16).slice(2)}`;

    const conceptA = mgRecords[0]?.concept_a ?? "";
    const conceptB = mgRecords[0]?.concept_b ?? "";

    const width = compact ? 250 : 980;
    const height = compact ? 270 : 840;

    const margin = compact
        ? { top: 28, right: 8, bottom: 8, left: 8 }
        : { top: 70, right: 30, bottom: 30, left: 90 };

    const svg = d3.select(card)
        .append("svg")
        .attr("viewBox", `0 0 ${width} ${height}`);

    svg.append("text")
        .attr("x", margin.left)
        .attr("y", compact ? 18 : 28)
        .attr("font-size", compact ? 12 : 14)
        .attr("font-weight", 600)
        .text(title);

    if (!mgRecords?.length || !usRecords?.length) {
        svg.append("text")
            .attr("x", margin.left)
            .attr("y", compact ? 38 : 55)
            .attr("font-size", 12)
            .text("No data.");
        return;
    }

    // Use a consistent color order; use combined records so "cluster" order is stable
    const combined = mgRecords.concat(usRecords);
    const colorOrder = computeColorOrder(combined, state);
    const idx = new Map(colorOrder.map((c, i) => [c, i]));

    // Symmetric ΔS lookup for each group
    const mgDist = new Map();
    for (const d of mgRecords) {
        mgDist.set(`${d.color_1}|||${d.color_2}`, d.semantic_distance);
        mgDist.set(`${d.color_2}|||${d.color_1}`, d.semantic_distance);
    }

    const usDist = new Map();
    for (const d of usRecords) {
        usDist.set(`${d.color_1}|||${d.color_2}`, d.semantic_distance);
        usDist.set(`${d.color_2}|||${d.color_1}`, d.semantic_distance);
    }

    // diagonal
    for (const c of colorOrder) {
        mgDist.set(`${c}|||${c}`, 0);
        usDist.set(`${c}|||${c}`, 0);
    }

    const innerW = width - margin.left - margin.right;
    const innerH = height - margin.top - margin.bottom;

    const x = d3.scaleBand().domain(colorOrder).range([0, innerW]);
    const y = d3.scaleBand().domain(colorOrder).range([0, innerH]);

    // Build tiles + gather diffs for scale
    const tiles = [];
    const diffs = [];

    for (const row of colorOrder) {
        for (const col of colorOrder) {
            const mgV = mgDist.get(`${row}|||${col}`);
            const usV = usDist.get(`${row}|||${col}`);
            const diff = usV - mgV; // US − MG

            diffs.push(diff);

            tiles.push({
                rowColor: row,
                colColor: col,
                mgV,
                usV,
                diff,
                rowName: COLOR_LOOKUP.get(row)?.name,
                colName: COLOR_LOOKUP.get(col)?.name
            });
        }
    }

    // Diverging grayscale centered at 0: negative -> darker, positive -> lighter
    const maxAbs = d3.max(diffs, d => Math.abs(d)) || 1;
    const fill = d3.scaleSequential(d3.interpolateGreys).domain([maxAbs, -maxAbs]);

    const g = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);

    if (!compact) {
        g.append("g")
            .attr("class", "axis")
            .call(d3.axisLeft(y).tickSize(0));

        g.append("g")
            .attr("class", "axis")
            .call(d3.axisTop(x).tickSize(0))
            .selectAll("text")
            .attr("text-anchor", "start")
            .attr("transform", "rotate(-90)")
            .attr("dx", "0.6em")
            .attr("dy", "0.6em");
    }

    g.append("g")
        .selectAll("rect")
        .data(tiles)
        .join("rect")
        .attr("x", t => x(t.colColor))
        .attr("y", t => y(t.rowColor))
        .attr("width", x.bandwidth())
        .attr("height", y.bandwidth())
        .attr("fill", t => {
            const r = idx.get(t.rowColor);
            const c = idx.get(t.colColor);
            const show = (state.half === "full") || (r <= c);

            if (show) return fill(t.diff);

            // faint placeholder for hidden bottom-half (same idea as your heatmap)
            return "rgba(107, 86, 86, 0.08)";
        })
        .attr("stroke", t => {
            const r = idx.get(t.rowColor);
            const c = idx.get(t.colColor);
            const show = (state.half === "full") || (r <= c);
            return show ? "none" : "rgba(130, 116, 116, 0.1)";
        })
        .style("pointer-events", t => {
            const r = idx.get(t.rowColor);
            const c = idx.get(t.colColor);
            const show = (state.half === "full") || (r <= c);
            return show ? "all" : "none";
        })
        .on("mousemove", (event, t) => {
            setTooltip(tooltipHTMLForDiff(t, conceptA, conceptB, cssForColorCode), event.clientX, event.clientY);
        })
        .on("mouseleave", hideTooltip)
        .on("click", function (event, t) {
            event.stopPropagation();
            const key = `${heatmapId}|||${t.rowColor}|||${t.colColor}`;
            togglePinnedTooltip(key, tooltipHTMLForDiff(t, conceptA, conceptB, cssForColorCode), event.clientX, event.clientY, this);
        });



    // Legend only for non-compact (like your other heatmap)
    if (!compact) {
        const legendW = 240;
        const legendH = 10;
        const legendX = margin.left;
        const legendY = height - 28;

        const legend = svg.append("g").attr("class", "legend");

        legend.append("text")
            .attr("x", legendX)
            .attr("y", legendY - 8)
            // .text(`diff scale (US−MG): ${(-maxAbs).toFixed(2)} → ${(maxAbs).toFixed(2)}`);
            .text(`diff scale (US−MG): dark = ${(-maxAbs).toFixed(2)}  •  light = ${(maxAbs).toFixed(2)}`);


        const gradId = `grad-diff-${Math.random().toString(16).slice(2)}`;
        const defs = svg.append("defs");
        const grad = defs.append("linearGradient")
            .attr("id", gradId)
            .attr("x1", "0%").attr("x2", "100%")
            .attr("y1", "0%").attr("y2", "0%");

        const stops = d3.range(0, 1.0001, 0.1);
        grad.selectAll("stop")
            .data(stops)
            .join("stop")
            .attr("offset", d => `${d * 100}%`)
            // .attr("stop-color", d => fill(maxAbs + d * (-2 * maxAbs))); // +maxAbs -> -maxAbs
            .attr("stop-color", d => fill(-maxAbs + d * (2 * maxAbs))); // -maxAbs -> +maxAbs (dark -> light)


        legend.append("rect")
            .attr("x", legendX)
            .attr("y", legendY)
            .attr("width", legendW)
            .attr("height", legendH)
            .attr("fill", `url(#${gradId})`)
            .attr("stroke", "#ccc");
    }
}

// ---------- distribution rendering----------
function renderDistribution(container, mg, us, title, compact = false) {
    container.innerHTML = "";

    const card = document.createElement("div");
    card.className = "card";
    container.appendChild(card);

    const width = compact ? 250 : 980;
    const height = compact ? 220 : 440;
    const margin = compact
        ? { top: 28, right: 10, bottom: 26, left: 32 }
        : { top: 50, right: 30, bottom: 40, left: 60 };

    const svg = d3.select(card)
        .append("svg")
        .attr("viewBox", `0 0 ${width} ${height}`);

    svg.append("text")
        .attr("x", margin.left)
        .attr("y", 18)
        .attr("font-size", compact ? 12 : 14)
        .attr("font-weight", 600)
        .text(title);

    const allVals = []
        .concat(mg.map(d => d.semantic_distance))
        .concat(us.map(d => d.semantic_distance));

    if (allVals.length === 0) {
        svg.append("text").attr("x", margin.left).attr("y", 40).attr("font-size", 12).text("No data.");
        return;
    }

    const innerW = width - margin.left - margin.right;
    const innerH = height - margin.top - margin.bottom;

    const x = d3.scaleLinear()
        .domain([0, d3.max(allVals)]).nice()
        .range([0, innerW]);

    const bins = d3.bin().domain(x.domain()).thresholds(compact ? 18 : 30);

    const mgBins = mg.length ? bins(mg.map(d => d.semantic_distance)) : [];
    const usBins = us.length ? bins(us.map(d => d.semantic_distance)) : [];

    const yMax = d3.max([].concat(mgBins, usBins), b => b.length) || 1;
    const y = d3.scaleLinear().domain([0, yMax]).nice().range([innerH, 0]);

    const g = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);

    g.append("g").attr("class", "axis")
        .attr("transform", `translate(0,${innerH})`)
        .call(d3.axisBottom(x).ticks(compact ? 4 : 8));

    g.append("g").attr("class", "axis")
        .call(d3.axisLeft(y).ticks(compact ? 3 : 6));


    const mgColor = d3.schemeTableau10[0];
    const usColor = d3.schemeTableau10[1];

    drawHist(mgBins, "MG", mgColor, g, x, y, innerH, hideTooltip);
    drawHist(usBins, "US", usColor, g, x, y, innerH, hideTooltip);
}

// ---------- main render ----------
function render(all) {
    const selected = state.selectedPairs.slice();
    const many = selected.length > 1;

    // meta
    const metaParts = [];
    metaParts.push(`<b>Selected pairs:</b> ${selected.length}`);
    metaParts.push(`<b>Dataset:</b> ${state.group.toUpperCase()}`);
    metaParts.push(`<b>View:</b> ${state.view}`);
    // if (many) metaParts.push(`<b>Layout:</b> 5 per row`);
    if (many) metaParts.push(`<b>Columns:</b> ${state.cols}`);
    els.meta.innerHTML = metaParts.join(" &nbsp;&nbsp;|&nbsp;&nbsp; ");

    els.viz.innerHTML = "";

    // 5 per row when multiple; otherwise single large
    if (many) {
        const grid = document.createElement("div");
        grid.className = "gridN";
        grid.style.setProperty("--cols", state.cols);
        els.viz.appendChild(grid);

        for (const pairKey of selected) {
            const cell = document.createElement("div");
            grid.appendChild(cell);

            const { A, B, mg, us } = getSubsetForPair(all, pairKey, state);
            const title = `${A} — ${B}`;

            if (state.view === "dist") {
                renderDistribution(cell, mg, us, title, true);
            } else {
                if (state.group === "both") {
                    cell.innerHTML = "";
                    const wrapper = document.createElement("div");
                    wrapper.className = "card";
                    cell.appendChild(wrapper);

                    const top = document.createElement("div");
                    const mid = document.createElement("div");
                    const bot = document.createElement("div");
                    wrapper.appendChild(top);
                    wrapper.appendChild(mid);
                    wrapper.appendChild(bot);

                    renderHeatmap(top, us, `${title} (US)`, true);
                    renderHeatmap(mid, mg, `${title} (MG)`, true);
                    renderDiffHeatmap(bot, mg, us, `${title} (Diff US−MG)`, true);

                } else if (state.group === "mg") {
                    renderHeatmap(cell, mg, title, true);
                } else {
                    renderHeatmap(cell, us, title, true);
                }
            }
        }

        return;
    }

    // single selection: keep the detailed large view
    if (selected.length === 0) return;
    const pairKey = selected[0];
    const { A, B, mg, us } = getSubsetForPair(all, pairKey, state);

    if (state.view === "dist") {
        renderDistribution(els.viz, mg, us, `Distribution — ${A} — ${B}`, false);
        return;
    }

    if (state.group === "both") {
        const row = document.createElement("div");
        row.style.display = "grid";
        row.style.gridTemplateColumns = "1fr 1fr 1fr";
        row.style.gap = "10px";
        els.viz.appendChild(row);

        const left = document.createElement("div");
        const mid = document.createElement("div");
        const right = document.createElement("div");
        row.appendChild(left);
        row.appendChild(mid);
        row.appendChild(right);

        renderHeatmap(left, us, `USA heatmap — ${A} — ${B}`, false);
        renderHeatmap(mid, mg, `MDG heatmap — ${A} — ${B}`, false);
        renderDiffHeatmap(right, mg, us, `Diff (US − MG) — ${A} — ${B}`, false);

    } else if (state.group === "mg") {
        renderHeatmap(els.viz, mg, `MDG heatmap — ${A} — ${B}`, false);
    } else {
        renderHeatmap(els.viz, us, `USA heatmap — ${A} — ${B}`, false);
    }
}
