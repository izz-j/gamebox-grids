(in-package :gamebox-grids)

(defclass hex-grid (grid-spec)
  ((forward :reader forward)
   (inverse :reader inverse)
   (offset :initarg :offset
           :initform :even))
  (:documentation "A hexagon grid definition."))

(defclass hex-rows (hex-grid)
  ((start-angle :initform 1)
   (forward :initform (quat (sqrt 3) (/ (sqrt 3) 2) 0.0 (/ 3.0 2.0)))
   (inverse :initform (quat (/ (sqrt 3) 3) (/ -1.0 3.0) 0.0 (/ 2.0 3.0)))
   (neighbor-dirs :initform '(:e :ne :nw :w :sw :se))
   (corner-dirs :initform '(:ne :n :nw :sw :s :se))
   (edge-dirs :initform '(:ne :nw :w :sw :se :e)))
  (:documentation "A row-based hexagon grid definition."))

(defclass hex-cols (hex-grid)
  ((forward :initform (quat (/ 3.0 2.0) 0.0 (/ (sqrt 3) 2) (sqrt 3)))
   (inverse :initform (quat (/ 2.0 3.0) 0.0 (/ -1.0 3.0) (/ (sqrt 3) 3)))
   (neighbor-dirs :initform '(:se :ne :n :nw :sw :s))
   (corner-dirs :initform '(:e :ne :nw :w :sw :se))
   (edge-dirs :initform '(:ne :n :nw :sw :s :se)))
  (:documentation "A column-based hexagon grid definition."))

(defmethod initialize-instance :after ((grid hex-cols) &key)
  ;; For column-based hexagon grids, we need to modify the neighbor directions and the grid offset
  ;; if the y-axis is up.
  (with-slots (neighbor-dirs y-axis offset) grid
    (when (eq y-axis :up)
      (setf neighbor-dirs (rotate neighbor-dirs))
      (swap-grid-offset grid))))

(defun grid-offset (grid)
  "Map a grid's offset keyword specifier to an integer."
  (with-slots (offset) grid
    (case offset
      (:even 1)
      (:odd -1))))

(defun swap-grid-offset (grid)
  "Change a grid's offset to even if it is odd, or odd if it is even."
  (with-slots (offset) grid
    (if (eq offset :even)
        (setf offset :odd)
        (setf offset :even))))

(defun hex (x y)
  "Calculate cube coordinates from grid coordinates."
  (vec x y (- (- x) y)))

(defgeneric hex-to-cell (grid hex)
  (:documentation "Convert a hexagon's cube coordinates to grid coordinates."))

(defmethod hex-to-cell ((grid hex-rows) hex)
  "For row-based hexagon grids, convert a hexagon's cube coordinates to grid coordinates."
  (with-vector (h hex)
    (let ((x (+ hx (/ (+ hy (* (grid-offset grid) (mod hy 2))) 2)))
          (y hy))
      (vec x y))))

(defmethod hex-to-cell ((grid hex-cols) hex)
  "For column-based hexagon grids, convert a hexagon's cube coordinates to grid coordinates."
  (with-vector (h hex)
    (let ((x hx)
          (y (+ hy (/ (+ hx (* (grid-offset grid) (mod hx 2))) 2))))
      (vec x y))))

(defgeneric hex-from-cell (grid cell)
  (:documentation "Convert a hexagon's grid coordinates to cube coordinates."))

(defmethod hex-from-cell ((grid hex-rows) cell)
  "For row-based hexagon grids, convert a hexagon's grid coordinates to cube coordinates."
  (with-vector (c cell)
    (let ((x (- cx (/ (+ cy (* (grid-offset grid) (mod cy 2))) 2)))
          (y cy))
      (hex x y))))

(defmethod hex-from-cell ((grid hex-cols) cell)
  "For column-based hexagon grids, convert a hexagon's grid coordinates to cube coordinates."
  (with-vector (c cell)
    (let ((x cx)
          (y (- cy (/ (+ cx (* (grid-offset grid) (mod cx 2))) 2))))
      (hex x y))))

(defun hex-round (hex)
  "Round a hexagon's cube coordinates to the nearest cell."
  (let* ((rounded (vround hex))
         (diff (vabs (v- rounded hex))))
    (with-vectors ((r rounded) (d diff))
      (cond ((and (> dx dy) (> dx dz))
             (setf rx (- (- ry) rz)))
            ((> dy dz)
             (setf ry (- (- rx) rz))))
      rounded)))

(defun hex-nudge (hex)
  "Apply an epsilon to a hexagon's cube coordinates to prevent interpolating on an edge."
  (v+ hex (hex 1e-7 1e-7)))

(defun hex-neighbor-offsets ()
  "A list of cardinal direction vectors representing a hexagon cell's neighbors."
  (list (hex 1 0)
        (hex 1 -1)
        (hex 0 -1)
        (hex -1 0)
        (hex -1 1)
        (hex 0 1)))

(defmethod cell-distance ((grid hex-grid) cell1 cell2)
  "The distance in cells between two particular cells."
  (with-vector (c (vabs (hex-from-cell grid (v- cell1 cell2))))
    (floor (max cx cy cz))))

(defmethod cell-to-point ((grid hex-grid) cell)
  "Get a point in screen coordinates of a particular cell."
  (with-slots (forward cell-size cell-origin) grid
    (with-vectors ((c (hex-from-cell grid cell)) (s cell-size))
      (with-quat (f forward)
        (let* ((x (+ (* fw cx) (* fx cy)))
               (y (+ (* fy cx) (* fz cy)))
               (px (* x sx))
               (py (* y sy (y-axis grid))))
          (vround (v+ (vec px py) cell-origin)))))))

(defmethod cell-from-point ((grid hex-grid) point)
  "Get a cell at a particular point in screen coordinates."
  (with-slots (inverse cell-size cell-origin) grid
    (with-vector (p (vhad/ (v- point cell-origin) (vhad* cell-size (vec 1 (y-axis grid)))))
      (with-quat (i inverse)
        (let* ((x (+ (* iw px) (* ix py)))
               (y (+ (* iy px) (* iz py))))
          (hex-to-cell grid (hex-round (hex x y))))))))

(defmethod cell-select-line ((grid hex-grid) cell1 cell2)
  "Get a list of cells that intersect a line drawn from one cell to another."
  (loop :with distance = (cell-distance grid cell1 cell2)
        :with step = (/ 1.0 (max distance 1))
        :with start = (hex-nudge (hex-from-cell grid cell1))
        :with end = (hex-nudge (hex-from-cell grid cell2))
        :for i :to distance
        :collect (hex-to-cell grid (hex-round (vlerp start end (* step i))))))

(defmethod cell-select-range ((grid hex-grid) cell range)
  "Get a list of cells up to a certain range of cells away from a cell."
  (loop :for x :from (- range) :to range
        :for min = (max (- range) (- (- x) range))
        :for max = (min range (+ (- x) range))
        :append (loop :for y :from min :to max
                      :for hex = (v+ (hex-from-cell grid cell) (vec x y))
                      :collect (hex-to-cell grid hex))))

(defmethod cell-neighbor-by-index ((grid hex-grid) cell index)
  "Low-level function to get a cell's neighbor given an index."
  (hex-to-cell grid (v+ (hex-from-cell grid cell) (elt (hex-neighbor-offsets) index))))
