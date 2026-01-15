/* global d3 */

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
};

const els = {
    groupSelect: document.getElementById("groupSelect"),
    pairSelect: document.getElementById("pairSelect"),
    allPairs: document.getElementById("allPairs"),
    clearPairs: document.getElementById("clearPairs"),
    viewSelect: document.getElementById("viewSelect"),
    orderSelect: document.getElementById("orderSelect"),
    viz: document.getElementById("viz"),
    meta: document.getElementById("meta"),
    tooltip: d3.select("#tooltip"),
    colsInput: document.getElementById("colsInput"),
};

// ---------- helpers ----------
function normalizePair(a, b) {
    return (a < b) ? `${a}|||${b}` : `${b}|||${a}`;
}

function parseRow(r, countryOverride) {
    return {
        country: countryOverride ?? r.country,
        concept_a: r.concept_a,
        concept_b: r.concept_b,
        color_1: r.color_1,
        color_2: r.color_2,
        semantic_distance: +r.semantic_distance,
    };
}

function setTooltip(html, x, y) {
    els.tooltip
        .style("opacity", 1)
        .html(html)
        .style("left", `${x + 12}px`)
        .style("top", `${y + 12}px`);
}

function hideTooltip() {
    els.tooltip.style("opacity", 0);
}

function getSelectedPairKeys() {
    return Array.from(els.pairSelect.selectedOptions).map(o => o.value);
}

function setSelectedPairKeys(keys) {
    const keySet = new Set(keys);
    for (const opt of els.pairSelect.options) {
        opt.selected = keySet.has(opt.value);
    }
}

function clamp255(x) {
    const v = Math.round(x);
    return Math.max(0, Math.min(255, v));
}

function cssForColorCode(code) {
    const d = COLOR_LOOKUP.get(code);
    if (!d || !d.hex) return "#fff";  // fallback
    // Ensure it starts with '#'
    // return d.hex.startsWith("#") ? d.hex : `#${d.hex}`;
    return d.hex;
}

function labcLine(code) {
    const d = COLOR_LOOKUP.get(code);
    if (!d) return `${code}: (no Labc found)`;
    return `${code}: L=${d.L.toFixed(1)} a=${d.a.toFixed(1)} b=${d.b.toFixed(1)} c=${d.c.toFixed(1)}`;
    // return `${code}: L=${d.L.toFixed(1)} a=${d.a.toFixed(1)} b=${d.b.toFixed(1)} c=${d.c.toFixed(1)}`;
}

function heatmapTooltipHTML(d) {
    const c1Css = cssForColorCode(d.c1);
    const c2Css = cssForColorCode(d.c2);
    return `
    <div><b>${d.c1} × ${d.c2}</b></div>
    <div>&Delta;S: <b>${d.d.toFixed(4)}</b></div>

    <div style="display:flex; gap:10px; margin-top:8px; align-items:flex-start;">
      <div style="display:flex; flex-direction:column; gap:6px; align-items:center;">
        <div style="width:50px;height:50px;background:${c1Css};border:1px solid rgba(255,255,255,0.25);"></div>
        <div style="font-size:11px; opacity:0.9;">${d.c1} <br/> ${d.name}</div>
      </div>

      <div style="display:flex; flex-direction:column; gap:6px; align-items:center;">
        <div style="width:50px;height:50px;background:${c2Css};border:1px solid rgba(255,255,255,0.25);"></div>
        <div style="font-size:11px; opacity:0.9;">${d.c2} <br/> ${d.name}</div>
      </div>
    </div>

    <div style="margin-top:8px; font-size:11px; opacity:0.9; line-height:1.25;">
      <div>${labcLine(d.c1)}</div>
      <div>${labcLine(d.c2)}</div>
    </div>
  `;
}


// key -> HTMLElement for pinned tooltips
const PINNED = new Map();

function pinnedKey(heatmapId, c1, c2) {
    return `${heatmapId}|||${c1}|||${c2}`;
}

function makePinnedTooltip(key, html, x, y) {
    const div = document.createElement("div");
    div.className = "tooltip pinned";
    div.dataset.key = key;
    div.style.opacity = 1;
    div.style.left = `${x + 12}px`;
    div.style.top = `${y + 12}px`;
    div.innerHTML = html;

    // Optional: click the pinned tooltip itself to close it
    div.addEventListener("click", (e) => {
        e.stopPropagation();
        removePinnedTooltip(key);
    });

    document.body.appendChild(div);
    return div;
}

function removePinnedTooltip(key) {
    const el = PINNED.get(key);
    if (el) {
        el.remove();
        PINNED.delete(key);
    }
}

function togglePinnedTooltip(key, html, x, y) {
    if (PINNED.has(key)) {
        removePinnedTooltip(key);
    } else {
        const el = makePinnedTooltip(key, html, x, y);
        PINNED.set(key, el);
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

// ---------- subset for a given pairKey ----------
function getSubsetForPair(all, pairKey) {
    const [A, B] = pairKey.split("|||");
    const byPair = all.filter(d => normalizePair(d.concept_a, d.concept_b) === pairKey);

    if (state.group === "mg") return { A, B, mg: byPair.filter(d => d.country === "mg"), us: [] };
    if (state.group === "us") return { A, B, mg: [], us: byPair.filter(d => d.country === "us") };
    return { A, B, mg: byPair.filter(d => d.country === "mg"), us: byPair.filter(d => d.country === "us") };
}

// ---------- ordering for colors ----------
function computeColorOrder(records) {
    const colors = Array.from(new Set(records.flatMap(d => [d.color_1, d.color_2]))).sort();
    if (state.order === "alpha") return colors;

    // heuristic: mean distance per color
    const sums = new Map(colors.map(c => [c, { sum: 0, n: 0 }]));
    for (const d of records) {
        sums.get(d.color_1).sum += d.semantic_distance; sums.get(d.color_1).n += 1;
        sums.get(d.color_2).sum += d.semantic_distance; sums.get(d.color_2).n += 1;
    }
    return colors
        .map(c => {
            const s = sums.get(c);
            return { c, mean: s.n ? s.sum / s.n : 0 };
        })
        .sort((a, b) => a.mean - b.mean)
        .map(x => x.c);
}

// ---------- heatmap rendering (supports compact small-multiples) ----------
function renderHeatmap(container, records, title, compact = false) {
    const heatmapId = `hm-${Math.random().toString(16).slice(2)}`;

    container.innerHTML = "";

    const card = document.createElement("div");
    card.className = "card";
    container.appendChild(card);

    // compact sizes for small multiples
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

    const colorOrder = computeColorOrder(records);

    // symmetric lookup
    const dist = new Map();
    for (const d of records) {
        dist.set(`${d.color_1}|||${d.color_2}`, d.semantic_distance);
        dist.set(`${d.color_2}|||${d.color_1}`, d.semantic_distance);
    }
    for (const c of colorOrder) dist.set(`${c}|||${c}`, 0);

    const innerW = width - margin.left - margin.right;
    const innerH = height - margin.top - margin.bottom;

    const x = d3.scaleBand().domain(colorOrder).range([0, innerW]);
    const y = d3.scaleBand().domain(colorOrder).range([0, innerH]);

    const vals = [];
    for (const c1 of colorOrder) for (const c2 of colorOrder) vals.push(dist.get(`${c1}|||${c2}`));
    const vmin = d3.min(vals);
    const vmax = d3.max(vals);

    const fill = d3.scaleSequential(d3.interpolateViridis).domain([vmin, vmax]);

    const g = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);

    // axes only when not compact
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
            .attr("dy", "-0.2em");
    }

    const tiles = [];
    for (const c1 of colorOrder) {
        for (const c2 of colorOrder) {
            tiles.push({ c1, c2, d: dist.get(`${c1}|||${c2}`), name: COLOR_LOOKUP.get(c1)?.name });
        }
    }

    g.append("g")
        .selectAll("rect")
        .data(tiles)
        .join("rect")
        .attr("x", d => x(d.c1))
        .attr("y", d => y(d.c2))
        .attr("width", x.bandwidth())
        .attr("height", y.bandwidth())
        .attr("fill", d => fill(d.d))
        .on("mousemove", (event, d) => {
            // transient hover tooltip
            setTooltip(heatmapTooltipHTML(d), event.clientX, event.clientY);
        })
        .on("mouseleave", () => {
            // hide only the transient tooltip; pinned ones remain
            hideTooltip();
        })
        .on("click", (event, d) => {
            // toggle pin for THIS cell; allows multiple pinned tooltips across heatmaps
            event.stopPropagation();
            const key = pinnedKey(heatmapId, d.c1, d.c2);
            togglePinnedTooltip(key, heatmapTooltipHTML(d), event.clientX, event.clientY);
        });

    // legend only for large view (keeps small multiples clean)
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

// ---------- distribution rendering (compact-friendly) ----------
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

    function drawHist(binData, label, color) {
        if (!binData.length) return;

        g.append("g")
            .selectAll("rect")
            .data(binData)
            .join("rect")
            .attr("x", d => x(d.x0))
            .attr("y", d => y(d.length))
            .attr("width", d => Math.max(0, x(d.x1) - x(d.x0) - 1))
            .attr("height", d => innerH - y(d.length))
            .attr("fill", color)
            .attr("opacity", 0.45)
            .on("mousemove", (event, d) => {
                setTooltip(
                    `<div><b>${label}</b></div>
           <div>range: ${d.x0.toFixed(2)}–${d.x1.toFixed(2)}</div>
           <div>count: <b>${d.length}</b></div>`,
                    event.clientX, event.clientY
                );
            })
            .on("mouseleave", hideTooltip);
    }

    const mgColor = d3.schemeTableau10[0];
    const usColor = d3.schemeTableau10[1];

    drawHist(mgBins, "MG", mgColor);
    drawHist(usBins, "US", usColor);
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

            const { A, B, mg, us } = getSubsetForPair(all, pairKey);
            const title = `${A} — ${B}`;

            if (state.view === "dist") {
                renderDistribution(cell, mg, us, title, true);
            } else {
                if (state.group === "both") {
                    // stack MG + US compact heatmaps in the same small card cell
                    cell.innerHTML = "";
                    const wrapper = document.createElement("div");
                    wrapper.className = "card";
                    cell.appendChild(wrapper);

                    const top = document.createElement("div");
                    const bot = document.createElement("div");
                    wrapper.appendChild(top);
                    wrapper.appendChild(bot);

                    renderHeatmap(top, mg, `${title} (MG)`, true);
                    renderHeatmap(bot, us, `${title} (US)`, true);
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
    const { A, B, mg, us } = getSubsetForPair(all, pairKey);

    if (state.view === "dist") {
        renderDistribution(els.viz, mg, us, `Distribution — ${A} — ${B}`, false);
        return;
    }

    if (state.group === "both") {
        const row = document.createElement("div");
        row.style.display = "grid";
        row.style.gridTemplateColumns = "1fr 1fr";
        row.style.gap = "12px";
        els.viz.appendChild(row);

        const left = document.createElement("div");
        const right = document.createElement("div");
        row.appendChild(left);
        row.appendChild(right);

        renderHeatmap(left, mg, `MG heatmap — ${A} — ${B}`, false);
        renderHeatmap(right, us, `US heatmap — ${A} — ${B}`, false);
    } else if (state.group === "mg") {
        renderHeatmap(els.viz, mg, `MG heatmap — ${A} — ${B}`, false);
    } else {
        renderHeatmap(els.viz, us, `US heatmap — ${A} — ${B}`, false);
    }
}
