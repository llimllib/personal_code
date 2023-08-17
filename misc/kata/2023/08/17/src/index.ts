import { colorFromRange, setAlpha, srgb } from "@thi.ng/color";
import { downloadCanvas } from "@thi.ng/dl-asset";
import { curve } from "@thi.ng/dsp";
import { Fiber, fiber, untilPromise, wait } from "@thi.ng/fibers";
import { circle, Group, group, text } from "@thi.ng/geom";
import { draw } from "@thi.ng/hiccup-canvas";
import { button, canvas, div } from "@thi.ng/hiccup-html";
import { roundTo } from "@thi.ng/math";
import { SYSTEM as rand } from "@thi.ng/random";
import { $compile } from "@thi.ng/rdom";
import { cycle, range, symmetric, zip } from "@thi.ng/transducers";

const SIZE = 640;
// number of segments to cycle through
const NUM = 5;
let W = SIZE / NUM;
const SNAP = 16;

let WIDTH = window.innerWidth;
let HEIGHT = window.innerHeight;

// main animation co-routine, repeatedly spawing new toplevel column cells
const cellAnim = (scene: Group, delay: number) =>
  function* main(ctx: Fiber) {
    // infinite loop over [0..5) interval
    for (let x of cycle(range(NUM))) {
      // each cell as its own child process
      ctx.fork(bubble(scene, WIDTH / 2, HEIGHT / 2));
      ctx.fork(bubble(scene, WIDTH / 2, HEIGHT / 2));
      ctx.fork(bubble(scene, WIDTH / 2, HEIGHT / 2));
      ctx.fork(bubble(scene, WIDTH / 2, HEIGHT / 2));
      ctx.fork(bubble(scene, WIDTH / 2, HEIGHT / 2));
      ctx.fork(bubble(scene, WIDTH / 2, HEIGHT / 2));
      ctx.fork(bubble(scene, WIDTH / 2, HEIGHT / 2));
      ctx.fork(bubble(scene, WIDTH / 2, HEIGHT / 2));
      ctx.fork(bubble(scene, WIDTH / 2, HEIGHT / 2));
      yield* wait(delay);
    }
  };

const bubble = (scene: Group, x: number, y: number) =>
  function* (ctx: Fiber) {
    const col = srgb(colorFromRange("warm"));
    const velocity = 10;
    const steps = 200;
    const dir = [
      (rand.int() % velocity) - velocity / 2,
      (rand.int() % velocity) - velocity / 2,
    ];
    const attribs = {
      fill: setAlpha(null, col, 1 - Math.pow(Math.random(), 2)),
    };
    for (let i = 0; i < steps; i++) {
      scene.add(circle([x + dir[0] * i, y + dir[1] * i], 10, attribs));
      yield;
    }
  };

// fiber to clear given geometry group on each iteration
function* beginFrame(scene: Group) {
  while (true) {
    scene.clear();
    yield;
  }
}

// fiber to repeatedly draw given scene group to canvas
function* endFrame(canvas: HTMLCanvasElement, scene: Group) {
  const ctx = canvas.getContext("2d")!;
  while (true) {
    draw(ctx, scene);
    // draw(
    // 	ctx,
    // 	text([10, 20], `${scene.children.length + 4} fibers`, {
    // 		font: "12px sans-serif",
    // 		fill: "#000",
    // 	})
    // );
    yield;
  }
}

// create main fiber and later attach sub-processes
const app = fiber(function* (ctx) {
  window.addEventListener("resize", () => {
    WIDTH = window.innerWidth;
    HEIGHT = window.innerHeight;
  });

  // wait for DOM creation
  yield* untilPromise(
    $compile(
      div(
        {},
        canvas("#main.db", {
          width: WIDTH,
          height: HEIGHT,
        })
        // button(
        // 	{
        // 		onclick: () =>
        // 			downloadCanvas(main, `scene-${Date.now()}`),
        // 	},
        // 	"export"
        // )
      )
    ).mount(document.getElementById("app")!)
  );

  // get canvas ref
  const main = <HTMLCanvasElement>document.getElementById("main");
  // geometry group/container
  const scene = group({ __background: "#dcc" });

  // init child processes to create & draw animation
  ctx.forkAll(beginFrame(scene), cellAnim(scene, 10), endFrame(main, scene));

  // wait for children to complete (here: infinite)
  yield* ctx.join();
});

// kick-off
app.run();
