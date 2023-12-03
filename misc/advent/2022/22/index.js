import createNode from "./createnode.js";

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

const replacements = {
  "#": "🏔️",
  ".": "🟩",
  " ": "⬛️",
  "👀": "👀",
};

function processMap(map, width, height) {
  let loc = [0, 0];
  for (let row = 0; row < map.length; row++) {
    if (map[row].length < width) {
      map[row] += " ".repeat(width - map[row].length);
    }

    // dealing with emojis in the strings doesn't work well, because they are
    // multibyte sequences; so first convert the string to an array
    map[row] = Array.from(map[row]);

    // we are standing in the first unoccupied square in the first row
    if (row == 0) {
      for (let col = 0; col < width; col++) {
        if (map[row][col] == ".") {
          loc = [row, col];
          break;
        }
      }
    }

    // replace the map characters with emojis
    for (let col = 0; col < width; col++) {
      map[row][col] = replacements[map[row][col]];
    }
  }
  return [map, loc];
}

function drawMap(map, width, height, loc) {
  const table = document.querySelector("#map");
  for (let row = 0; row < height; row++) {
    const rowElt = createNode(`tr.row[data-row="${row}"]`);
    table.appendChild(rowElt);
    for (let col = 0; col < width; col++) {
      if (row == loc[0] && col == loc[1]) {
        const cell = createNode(
          `td.cell.loc[data-row="${row}"][data-col="${col}"] "${map[row][col]}"`
        );
        rowElt.appendChild(cell);
      } else {
        const cell = createNode(
          `td.cell[data-row="${row}"][data-col="${col}"] "${map[row][col]}"`
        );
        rowElt.appendChild(cell);
      }
    }
  }
}

const WALL = replacements["#"];
const OPEN = replacements["."];
const VOID = replacements[" "];
const EYES = "👀";

// We can use regular mod in the positive direction, but not in the negative,
// because javscript handles negative mod bizarrely
function mod(n, m) {
  return ((n % m) + m) % m;
}

const rotations = {
  RR: "D",
  RL: "U",
  DR: "L",
  DL: "R",
  LR: "U",
  LL: "D",
  UR: "R",
  UL: "L",
};

async function move(map, width, height, loc, steps, direction, rotation) {
  let n = 0;
  while (steps > 0) {
    n += 1;
    if (n > 1000000) {
      console.log("too much");
    }
    if (direction == "R") {
      // first skip over any empty spaces
      let l = [...loc];
      while (map[l[0]][(l[1] + 1) % width] == VOID) {
        l = [l[0], (l[1] + 1) % width];
      }

      // if the next space is a wall, don't change anything
      if (map[l[0]][(l[1] + 1) % width] == WALL) break;

      // if it's an open spot, move there
      if (map[l[0]][(l[1] + 1) % width] == OPEN) {
        loc = [l[0], (l[1] + 1) % width];
        await updateMap(loc);
        steps -= 1;
        continue;
      }
    } else if (direction == "L") {
      let l = [...loc];
      while (map[l[0]][mod(l[1] - 1, width)] == VOID) {
        l = [l[0], mod(l[1] - 1, width)];
      }

      if (map[l[0]][mod(l[1] - 1, width)] == WALL) break;

      if (map[l[0]][mod(l[1] - 1, width)] == OPEN) {
        loc = [l[0], mod(l[1] - 1, width)];
        await updateMap(loc);
        steps -= 1;
        continue;
      }
    } else if (direction == "D") {
      let l = [...loc];
      while (map[(l[0] + 1) % height][l[1]] == VOID) {
        l = [(l[0] + 1) % height, l[1]];
      }

      if (map[(l[0] + 1) % height][l[1]] == WALL) break;

      if (map[(l[0] + 1) % height][l[1]] == OPEN) {
        loc = [(l[0] + 1) % height, l[1]];
        await updateMap(loc);
        steps -= 1;
        continue;
      }
    } else if (direction == "U") {
      let l = [...loc];
      while (map[mod(l[0] - 1, height)][l[1]] == VOID) {
        l = [mod(l[0] - 1, height), l[1]];
      }

      if (map[mod(l[0] - 1, height)][l[1]] == WALL) break;

      if (map[mod(l[0] - 1, height)][l[1]] == OPEN) {
        loc = [mod(l[0] - 1, height), l[1]];
        await updateMap(loc);
        steps -= 1;
        continue;
      }
    } else {
      throw "invalid direction " + direction;
    }
  }

  return [loc, rotations[direction + rotation]];
}

async function updateMap(loc) {
  const cur = document.querySelector(".loc");
  cur.classList.remove("loc");
  cur.innerHTML = OPEN;

  const newSpot = document.querySelector(
    `td[data-row="${loc[0]}"][data-col="${loc[1]}"]`
  );
  newSpot.innerHTML = EYES;
  newSpot.classList.add("loc");
  newSpot.classList.add("visited");

  await sleep(1);
}

const values = {
  R: 0,
  D: 1,
  L: 2,
  U: 3,
};

async function run(map, width, height, loc, directions) {
  let direction = "R";
  for (let i = 0; i < directions.length; i++) {
    if (i % 100 == 0) {
      console.log(`${i} of ${directions.length}`);
    }
    let [_, n, rot] = directions[i].match(/(\d+)(\w)/);
    const steps = parseInt(n);
    [loc, direction] = await move(
      map,
      width,
      height,
      loc,
      steps,
      direction,
      rot
    );
  }
  console.log(
    "final password",
    loc,
    direction,
    1000 * (loc[0] + 1) + 4 * (loc[1] + 1) + values[direction]
  );
}

async function newMap() {
  const mapTable = document.querySelector("#map");
  mapTable.innerHTML = "";

  const puzzle = document.querySelector("#puzzle").value.trimEnd();
  let lines = puzzle.split("\n");
  const directions = lines.pop().match(/(\d+\w)/g);
  // remove the empty line at the bottom of the puzzle
  lines.pop();
  const width = Math.max(...lines.map((x) => x.length));
  const height = lines.length;

  let [map, loc] = processMap(lines, width, height);

  drawMap(map, width, height, loc);
  await run(map, width, height, loc, directions);
}

document.addEventListener("DOMContentLoaded", async () => {
  await newMap();
  document.querySelector("#puzzle").addEventListener("change", newMap);
});
