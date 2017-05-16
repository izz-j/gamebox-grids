(defpackage #:gamebox-grids
  (:use #:cl
        #:alexandria
        #:gamebox-math)
  (:export #:grid
           #:cell-member-p
           #:cell-distance
           #:cell-select-line
           #:cell-select-range
           #:cell-to-point
           #:cell-from-point
           #:cell-corner-directions
           #:cell-edge-directions
           #:cell-neighbors
           #:cell-neighbors-p
           #:cell-neighbor
           #:quad-4
           #:quad-8
           #:hex-rows
           #:hex-cols))
