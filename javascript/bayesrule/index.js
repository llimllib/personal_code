function calculatePosttestProbability() {
  const pretestProbability = parseFloat(
    document.getElementById("pretestProbability").value,
  );
  const likelihoodRatio = parseFloat(
    document.getElementById("likelihoodRatio").value,
  );

  if (isNaN(pretestProbability) || isNaN(likelihoodRatio)) {
    return "";
  } else {
    const prob =
      (pretestProbability * likelihoodRatio) /
      (pretestProbability * likelihoodRatio + (1 - pretestProbability));
    return `${Math.round(prob * 10000) / 100}%`;
  }
}

function update() {
  console.log("change");
  document.getElementById("result").textContent =
    calculatePosttestProbability();
}

document.addEventListener("DOMContentLoaded", () => {
  update();
  document
    .getElementById("pretestProbability")
    .addEventListener("input", update);
  document.getElementById("likelihoodRatio").addEventListener("input", update);
});
