function init_empty_matrix(group, matrixSize) {
    group.append("rect")
        .attr("x", 0)
        .attr("y", 20)
        .attr("width", matrixSize)
        .attr("height", matrixSize)
        .attr("fill", "lightgrey")
        .attr("stroke", "black")
        .attr("stroke-width", 1);

    // Draw center lines
    group.append("line")
        .attr("x1", matrixSize / 2).attr("y1", 20)
        .attr("x2", matrixSize / 2).attr("y2", matrixSize + 20)
        .attr("stroke", "black").attr("stroke-width", 1);
    group.append("line")
        .attr("x1", 0).attr("y1", matrixSize / 2 + 20)
        .attr("x2", matrixSize).attr("y2", matrixSize / 2 + 20)
        .attr("stroke", "black").attr("stroke-width", 1);

    // Labels for the axes
    group.append("text").attr("x", matrixSize / 4 - 20).attr("y", 15).text("mgL");
    group.append("text").attr("x", 3 * matrixSize / 4 - 20).attr("y", 15).text("mgH");
    group.append("text").attr("x", -10).attr("y", 15 + matrixSize / 4).text("usH")
        .attr("transform", `rotate(-90, -5, ${15 + matrixSize / 4})`);
    group.append("text").attr("x", -10).attr("y", 3 * matrixSize / 4).text("usL")
        .attr("transform", `rotate(-90, -5, ${3 * matrixSize / 4})`);
}
