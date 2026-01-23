export const els = {
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
    halfSelect: document.getElementById("halfSelect"),
};

export function normalizePair(a, b) {
    return (a < b) ? `${a}|||${b}` : `${b}|||${a}`;
}

export function parseRowOld(r, countryOverride) {
    return {
        country: countryOverride ?? r.country,
        concept_a: r.concept_a,
        concept_b: r.concept_b,
        color_1: r.color_1,
        color_2: r.color_2,
        semantic_distance: +r.semantic_distance,
        mu_D: +r.mu_D,
    };
}

export function parseRow(r, countryOverride) {
    return {
        country: countryOverride ?? r.country,
        concept_a: r.concept_a,
        concept_b: r.concept_b,
        color_1: r.color_1,
        color_2: r.color_2,
        semantic_distance: +r.semantic_distance,
        mu_D: +r.mu_D,

        // NEW:
        A_to_C1: +r.A_to_C1,
        A_to_C2: +r.A_to_C2,
        B_to_C1: +r.B_to_C1,
        B_to_C2: +r.B_to_C2,
    };
}


export function getSelectedPairKeys() {
    return Array.from(els.pairSelect.selectedOptions).map(o => o.value);
}

export function setSelectedPairKeys(keys) {
    const keySet = new Set(keys);
    for (const opt of els.pairSelect.options) {
        opt.selected = keySet.has(opt.value);
    }
}

export function drawHist(binData, label, color, g, x, y, innerH, hideTooltip) {
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


export function tooltipHTMLForCell(t, records, cssForColorCode) {
    const c1Css = cssForColorCode(t.rowColor);
    const c2Css = cssForColorCode(t.colColor);

    const conceptA = records[0]?.concept_a ?? "";
    const conceptB = records[0]?.concept_b ?? "";

    // 40x40 squares; moved down by 20px (squareY includes the extra 20)
    const W = 230;
    const H = 155;
    const s = 40;

    const leftX = 30;
    const rightX = 140;

    const leftCx = leftX + s / 2;   // 50
    const rightCx = rightX + s / 2; // 160

    const labelY = 14;
    const lineStartY = 26;

    // squares moved down by +20px
    const squareY = 60 + 20;

    // endpoints
    const leftTopMid = { x: leftCx, y: squareY };
    const rightTopMid = { x: rightCx, y: squareY };

    // nearest corners for crossings
    const rightTopLeft = { x: rightX, y: squareY };        // for A_to_col
    const leftTopRight = { x: leftX + s, y: squareY };     // for B_to_row

    // Scale line widths "proportionate" to the values in the 4 columns
    const vals = [
        t.A_to_row, t.A_to_col, t.B_to_row, t.B_to_col
    ].map(v => (Number.isFinite(v) ? v : 0));

    const maxV = Math.max(0, ...vals);
    const w = (v) => {
        const vv = Number.isFinite(v) ? v : 0;
        if (maxV <= 0) return 0.5;
        return 0.5 + 5 * (vv / maxV); // 0.5..4.0
    };

    const stroke = "rgba(255,255,255,0.9)";

    return `
    <div>Row (color1): <b> ${t.rowColor}</b></div>
    <div>Col (color2): <b> ${t.colColor}</b></div>
    <div style="margin-top:4px;">&Delta;S: <b>${t.d.toFixed(4)}</b></div>
    <div>&Delta;x = <b>${t.deltaX === null ? "NA" : t.deltaX.toFixed(4)}</b></div>

    <svg width="${W}" height="${H}" style="display:block; margin-top:8px;">
      <!-- concept labels -->
      <text x="${leftCx}" y="${labelY}" text-anchor="middle"
            style="font-size:12px; font-weight:600; fill:white;">${conceptA}</text>
      <text x="${rightCx}" y="${labelY}" text-anchor="middle"
            style="font-size:12px; font-weight:600; fill:white;">${conceptB}</text>

      <!-- 4 association lines -->
      <!-- Vertical: A_to_row -->
      <line x1="${leftCx}" y1="${lineStartY}" x2="${leftTopMid.x}" y2="${leftTopMid.y}"
            stroke="${stroke}" stroke-width="${w(t.A_to_row)}" stroke-linecap="round" />
      <!-- Vertical: B_to_col -->
      <line x1="${rightCx}" y1="${lineStartY}" x2="${rightTopMid.x}" y2="${rightTopMid.y}"
            stroke="${stroke}" stroke-width="${w(t.B_to_col)}" stroke-linecap="round" />

      <!-- Crossing: A_to_col ends on nearest corner of opposite square (right top-left) -->
      <line x1="${leftCx}" y1="${lineStartY}" x2="${rightTopLeft.x}" y2="${rightTopLeft.y}"
            stroke="${stroke}" stroke-width="${w(t.A_to_col)}" stroke-linecap="round" />
      <!-- Crossing: B_to_row ends on nearest corner of opposite square (left top-right) -->
      <line x1="${rightCx}" y1="${lineStartY}" x2="${leftTopRight.x}" y2="${leftTopRight.y}"
            stroke="${stroke}" stroke-width="${w(t.B_to_row)}" stroke-linecap="round" />

      <!-- squares -->
      <rect x="${leftX}" y="${squareY}" width="${s}" height="${s}"
            fill="${c1Css}" stroke="rgba(255,255,255,0.25)" />
      <rect x="${rightX}" y="${squareY}" width="${s}" height="${s}"
            fill="${c2Css}" stroke="rgba(255,255,255,0.25)" />

      <!-- color labels under squares -->
      <text x="${leftCx}" y="${squareY + s + 14}" text-anchor="middle"
            style="font-size:11px; fill:white;">${t.rowColor}</text>
      <text x="${rightCx}" y="${squareY + s + 14}" text-anchor="middle"
            style="font-size:11px; fill:white;">${t.colColor}</text>

      <text x="${leftCx}" y="${squareY + s + 28}" text-anchor="middle"
            style="font-size:10px; fill:rgba(255,255,255,0.9);">
        ${t.rowName ?? ""}
      </text>
      <text x="${rightCx}" y="${squareY + s + 28}" text-anchor="middle"
            style="font-size:10px; fill:rgba(255,255,255,0.9);">
        ${t.colName ?? ""}
      </text>
    </svg>
  `;
}


export function tooltipHTMLForDiff(t, conceptA, conceptB, cssForColorCode) {
    const c1Css = cssForColorCode(t.rowColor);
    const c2Css = cssForColorCode(t.colColor);

    const rowTag = conceptA ? ` <span style="opacity:0.95;">&lt;&gt; ${conceptA}</span>` : "";
    const colTag = conceptB ? ` <span style="opacity:0.95;">&lt;&gt; ${conceptB}</span>` : "";

    return `
          <div>Row (color1): <b> ${t.rowColor}${rowTag} </b></div>
          <div>Col (color2): <b> ${t.colColor}${colTag}</b></div>
          <div style="margin-top:4px;">US &Delta;S: <b>${t.usV.toFixed(4)}</b></div>
          <div>MG &Delta;S: <b>${t.mgV.toFixed(4)}</b></div>
          <div><b>Diff (US − MG): ${t.diff.toFixed(4)}</b></div>

          <div style="display:flex; gap:10px; margin-top:8px; align-items:flex-start;">
            <div style="display:flex; flex-direction:column; gap:6px; align-items:center;">
              <div style="width:30px;height:30px;background:${c1Css};border:1px solid rgba(255,255,255,0.25);"></div>
              <div style="font-size:11px; opacity:0.9;">${t.rowColor} <br/> ${t.rowName}</div>
            </div>

            <div style="display:flex; flex-direction:column; gap:6px; align-items:center;">
              <div style="width:30px;height:30px;background:${c2Css};border:1px solid rgba(255,255,255,0.25);"></div>
              <div style="font-size:11px; opacity:0.9;">${t.colColor} <br/> ${t.colName}</div>
            </div>
          </div>
        `;
}