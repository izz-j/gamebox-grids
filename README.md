# gamebox-grids

Create and manipulate tiles in a two-dimensional grid layout.

## Overview

This library allows you to represent cells of a grid, and perform operations on them which are
common to game development. Currently 4 different square grids and 8 different hexagonal grids are
supported. With them, you can perform such queries as calculating the distance between any two given
cells, listing all neighboring cells, and more.

## Install

This system is not yet available to be installed automatically
with [Quicklisp](https://www.quicklisp.org). To manually install using Quicklisp, clone this
repository into your `local-projects` directory and issue the following in your REPL:

``` lisp
(ql:quickload :gamebox-grids)
```

## Usage

To make use of this library, the first thing you'll need is to create a grid specification. To do so
you can use the `GRID` function, giving it a grid type, and optional arguments to control how the
grid behaves.

Some examples of a grid with square cells:

``` lisp
;; A grid of square cells, supporting 4 directions of movement, orthogonally.
(grid 'quad-4)

;; A grid of square cells, supporting 8 directions of movement, both orthogonally and diagonally.
(grid 'quad-8)

;; You can specify the width and height of the grid in cells. This will create a 20x20 grid.
(grid 'quad-8 :size (vec 20 20))

;; You can also specify the size in pixels of the cells. This is useful for converting a cell to
;; screen coordinates, and vice versa.
(grid 'quad-4 :cell-size (vec 32 32))

;; Even rectangular sizes are supported.
(grid 'quad-8 :cell-size (vec 12 4))

;; By default the Y axis is incremented downward. This means the grid origin cell (0,0) is at the
;; top left. We can change the Y axis to be up as such.
(grid 'quad-4 :cell-size (vec 16 16) :y-axis :up)
```

Hexagonal grids are also supported, with 8 different layouts. It's best to explain the different
layouts with a diagram:

![Hexagon Grid Diagram](https://img.axity.net/gamebox-grids-hex-layouts.png)

In the above image, the 8 different hexagonal grid layouts can be seen. On the left side, are 4
column-based layouts, and on the right side are 4 row-based layouts. For each of these, there is a
staggering or offset which is applied to even or odd columns or rows. When stagger is odd,
odd-numbered columns and rows are offset down and right, respectively. When stagger is even,
even-numbered columns and rows are offset down and right, respectively. Finally, there is the
Y-axis, which specifies how cell coordinates are assigned.

Some examples of hexagonal grids:

``` lisp
(grid 'hex-rows :offset :even :y-axis :down)
(grid 'hex-rows :offset :odd :y-axis :down
(grid 'hex-rows :offset :even :y-axis :up))
(grid 'hex-rows :offset :odd :y-axis :up)
(grid 'hex-cols :offset :even :y-axis :down)
(grid 'hex-cols :offset :odd :y-axis :down
(grid 'hex-cols :offset :even :y-axis :up))
(grid 'hex-cols :offset :odd :y-axis :up)
```

Just like a square grid, you can also specify the size of the grid and cells for hexagonal grids.

Once we have a grid, it would be nice to use it. You should first lexically bind it, or store the
object returned from the above `GRID` example somewhere to be accessed later:

``` lisp
(let ((grid (grid 'hex-rows :size (vec 10 10) :cell-size (vec 32 16))))

  ;; Test if a cell is a member of the grid. A cell with negative coordinates, or coordinates that
  ;; fall outside the size of the grid will not be a member. All others will, as we do not support
  ;; sparse grids at this time.
  (cell-member-p grid (cell 5 5)) ; => T

  ;; Find the distance from one cell to another.
  (cell-distance grid (cell 0 1) (cell 4 4)) ; => 6

  ;; If you were to drawn a line on the grid from the center of a cell to the center of any other
  ;; cell, this will find all cells that are touching the line.
  (cell-select-line grid (cell 1 0) (cell 2 3)) ; => (#(1.0 0.0 0.0) #(2.0 1.0 0.0) #(1.0 2.0 0.0)
                                                ;     #(2.0 3.0 0.0))

  ;; You can select all cells that surround a cell up to N steps away.
  (cell-select-range grid (cell 3 3) 1) ; => (#(2.0 3.0 0.0) #(2.0 4.0 0.0) #(2.0 2.0 0.0)
                                        ;     #(3.0 3.0 0.0) #(3.0 4.0 0.0) #(3.0 2.0 0.0)
                                        ;     #(4.0 3.0 0.0))

  ;; Find the screen coordinates in pixels of the center of a cell.
  (cell-to-point grid (cell 3 4)) ; => #(166.0 96.0 0.0)

  ;; Find the cell located at the given screen coordinates:
  (cell-from-point grid (vec 166 96)) ; => #(3.0 4.0 0.0)

  ;; Get a list of direction vectors from the center of a cell to each of its corners, keyed by an
  ;; approximate cardinal direction.
  (cell-corner-directions grid) ; => (:NE #<0.8660254 0.5 0.0> :N #<0.0 1.0 0.0>
                                ;     :NW #<-0.8660254 0.5 0.0> :SW #<-0.8660254 -0.5 0.0>
                                ;     :S #<0.0 -1.0 0.0> :SE #<0.8660254 -0.5 0.0>)

  ;; Get a list of direction vectors from the center of a cell to each of its edges, keyed by an
  ;; approximate cardinal direction.
  (cell-edge-directions grid) ; => (:NE #<0.5 0.8660254 0.0> :NW #<-0.5 0.8660254 0.0>
                              ;     :W #<-1.0 0.0 0.0> :SW #<-0.5 -0.8660254 0.0>
                              ;     :SE #<0.5 -0.8660254 0.0> :E #<1.0 0.0 0.0>)

  ;; Get a list of neighbors, keyed by their approximate cardinal direction from a cell.
  (cell-neighbors grid (cell 3 3)) ; => (:E #<4.0 3.0 0.0> :NE #<3.0 2.0 0.0> :NW #<2.0 2.0 0.0>
                                   ; :W #<2.0 3.0 0.0> :SW #<2.0 4.0 0.0> :SE #<3.0 4.0 0.0>)

  ;; Check whether or not two cells are neighbors.
  (cell-neighbors-p grid (cell 3 3) (cell 2 3)) ; => T

  ;; Get the neighbor of a cell given an approximate cardinal direction.
  (cell-neighbor grid (cell 3 3) :w) ; => #(2.0 3.0 0.0))
```

## License

Copyright © 2015 Michael Fiano <michael.fiano@gmail.com>.

Licensed under the MIT License.

A copy of the license is available [here](LICENSE).
