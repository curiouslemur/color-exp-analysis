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

export function parseRow(r, countryOverride) {
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

export function getSelectedPairKeys() {
    return Array.from(els.pairSelect.selectedOptions).map(o => o.value);
}

export function setSelectedPairKeys(keys) {
    const keySet = new Set(keys);
    for (const opt of els.pairSelect.options) {
        opt.selected = keySet.has(opt.value);
    }
}
