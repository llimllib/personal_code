-   cloned https://github.com/thi-ng/tpl-umbrella-basic
-   compiled two examples:
    -   [poisson-circles](https://github.com/thi-ng/umbrella/tree/develop/examples/poisson-circles)
    -   [fiber-basics](https://github.com/thi-ng/umbrella/tree/develop/examples/fiber-basics)
-   on the fibers example:
    -   managed to remove the "export" button
    -   got the canvas to go full screen
    -   failed to make the lines properly use the width and height of the screen
        -   the code deeply expects the surface to be a square, and taking apart the `cell` function was challenging for me