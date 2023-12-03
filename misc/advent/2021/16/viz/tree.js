import * as d3 from "d3-selection";
window.addEventListener("DOMContentLoaded", async (_evt) => {
  d3.graphviz("#graph")
      .renderDot('digraph {a -> b}');
});
