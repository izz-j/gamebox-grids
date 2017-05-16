(in-package :gamebox-grids)

(defclass grid-spec ()
  ((size :reader size
         :initarg :size
         :initform (vec 10 10))
   (y-axis :initarg :y-axis
           :initform :down)
   (cell-size :initarg :cell-size
              :initform (vec 1 1))
   (cell-origin :initarg :cell-origin
                :initform (vec))
   (start-angle :reader start-angle
                :initform 0)
   (neighbor-dirs :reader neighbor-dirs)
   (edge-dirs :reader edge-dirs)
   (corner-dirs :reader corner-dirs))
  (:documentation "A generic grid definition."))

(defmethod initialize-instance :after ((grid grid-spec) &key)
  ;; When the y-axis is up, we must re-arrange the neighbor directions.
  (with-slots (neighbor-dirs y-axis) grid
    (when (eq y-axis :up)
      (setf neighbor-dirs (rotate (reverse neighbor-dirs))))))

(defun grid (grid-type &rest args)
  "Define a grid specification given a grid type and options."
  (apply #'make-instance grid-type args))

(defun y-axis (grid)
  "Map a grid's y-axis keyword specifier to an integer."
  (with-slots (y-axis) grid
    (case y-axis
      (:down 1)
      (:up -1))))

(defun cell (x y)
  "Define a cell located at the specified coordinates in a grid."
  (vec x y))

(defgeneric cell-neighbor-offsets (grid)
  (:documentation "A list of cardinal direction vectors representing a cell's neighbors."))

(defgeneric cell-distance (grid cell1 cell2)
  (:documentation "The distance in cells between two particular cells."))

(defgeneric cell-to-point (grid cell)
  (:documentation "Get a point in screen coordinates of a particular cell."))

(defgeneric cell-from-point (grid point)
  (:documentation "Get a cell at a particular point in screen coordinates."))

(defgeneric cell-select-line (grid cell1 cell2)
  (:documentation "Get a list of cells that intersect a line drawn from one cell to another."))

(defgeneric cell-select-range (grid cell range)
  (:documentation "Get a list of cells up to a certain range of cells away from a cell."))

(defgeneric cell-neighbor-by-index (grid cell index))

(defun cell-nudge (cell)
  "Apply an epsilon to a cell's coordinates to prevent interpolating on an edge."
  (v+ cell (vec 1e-7 1e-7 1e-7)))

(defun cell-member-p (grid cell)
  "Test if a cell is a member of the specified grid."
  (with-vectors ((g (size grid)) (c cell))
    (and (>= cx 0)
         (< cx gx)
         (>= cy 0)
         (< cy gy))))

(defun cell-corner-count (grid)
  "The number of corners for a cell in the specified grid."
  (length (corner-dirs grid)))

(defun cell-edge-count (grid)
  "The number of edges for a cell in the specified grid."
  (length (edge-dirs grid)))

(defun cell-direction-count (grid)
  "The number of corners and edges combined for a cell in the specified grid."
  (+ (cell-corner-count grid) (cell-edge-count grid)))

(defun cell-directions (grid)
  "Get a list of direction vectors pointing from a cell's origin to each of its corners and edges."
  (with-slots (start-angle cell-size) grid
    (flet ((get-offset (count dir)
             (let* ((coeff (/ (+ start-angle dir) count))
                    (angle (lerp coeff 0 (* 2 pi))))
               (vhad* cell-size (vstab (vec (cos angle) (sin angle)))))))
      (loop :with dir-count = (cell-direction-count grid)
            :for dir :below dir-count
            :for offset = (get-offset dir-count dir)
            :collect (vnormalize offset)))))

(defun cell-corner-directions (grid)
  "Get a property list mapping cardinal directions to direction vectors for each corner of a cell."
  (mapcan #'list (corner-dirs grid)
          (loop :with dirs = (cell-directions grid)
                :for i :below (length dirs) :by 2
                :collect (elt dirs i))))

(defun cell-edge-directions (grid)
  "Get a property list mapping cardinal directions to direction vectors for each edge of a cell."
  (mapcan #'list (edge-dirs grid)
          (loop :with dirs = (cell-directions grid)
                :for i :from 1 :below (length dirs) :by 2
                :collect (elt dirs i))))

(defun cell-neighbors (grid cell)
  "Get a property list mapping cardinal directions to cell neighbors for a cell."
  (loop :for i :below (cell-edge-count grid)
        :for dir = (elt (neighbor-dirs grid) i)
        :for neighbor = (cell-neighbor-by-index grid cell i)
        :when (cell-member-p grid neighbor)
          :append (list dir neighbor)))

(defun cell-neighbors-p (grid cell1 cell2)
  "Test if two cells are neighbors in the specified grid."
  (loop :for i :below (cell-edge-count grid)
        :for dir = (elt (neighbor-dirs grid) i)
        :for neighbor = (cell-neighbor-by-index grid cell1 i)
        :do (when (v= neighbor cell2)
              (return t))))

(defun cell-neighbor (grid cell dir)
  "Get the neighbor of a cell in the specified cardinal direction."
  (getf (cell-neighbors grid cell) dir))
