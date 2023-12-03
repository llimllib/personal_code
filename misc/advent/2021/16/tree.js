import d3 as d3 from "d3";

window.addEventListener("DOMContentLoaded", async (_evt) => {
  d3.graphviz("#graph")
      .renderDot('digraph {a -> b}');
});
