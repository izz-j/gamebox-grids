(in-package :gamebox-grids)

(defclass quad-grid (grid-spec)
  ((start-angle :initform 1))
  (:documentation "A quad grid definition."))

(defclass quad-4 (quad-grid)
  ((neighbor-dirs :initform '(:e :n :w :s))
   (corner-dirs :initform '(:ne :nw :sw :se))
   (edge-dirs :initform '(:e :n :w :s)))
  (:documentation "A quad grid definition with 4-way movement."))

(defclass quad-8 (quad-grid)
  ((neighbor-dirs :initform '(:e :ne :n :nw :w :sw :s :se))
   (corner-dirs :initform '(:ne :nw :sw :se))
   (edge-dirs :initform '(:e :ne :n :nw :w :sw :s :se)))
  (:documentation "A quad grid definition with 8-way movement."))

(defmethod cell-neighbor-offsets ((grid quad-4))
  "A list of cardinal direction vectors representing a cell's neighbors."
  (list (vec 1 0)
        (vec 0 -1)
        (vec -1 0)
        (vec 0 1)))

(defmethod cell-neighbor-offsets ((grid quad-8))
  "A list of cardinal direction vectors representing a cell's neighbors."
  (list (vec 1 0)
        (vec 1 -1)
        (vec 0 -1)
        (vec -1 -1)
        (vec -1 0)
        (vec -1 1)
        (vec 0 1)
        (vec 1 1)))

(defmethod cell-distance ((grid quad-4) cell1 cell2)
  "The distance in cells between two particular cells on an 4-way quad grid."
  (with-vector (c (vpos (v- cell1 cell2)))
    (floor (+ cx cy))))

(defmethod cell-distance ((grid quad-8) cell1 cell2)
  "The distance in cells between two particular cells on an 8-way quad grid."
  (with-vector (c (vpos (v- cell1 cell2)))
    (floor (max cx cy))))

(defmethod cell-to-point ((grid quad-grid) cell)
  "Get a point in screen coordinates of a particular cell."
  (with-slots (cell-size cell-origin) grid
    (with-vectors ((c cell) (s cell-size))
      (let ((px (* cx sx))
            (py (* cy sy (y-axis grid))))
        (vround (v+ (vec px py) cell-origin))))))

(defmethod cell-from-point ((grid quad-grid) point)
  "Get a cell at a particular point in screen coordinates."
  (with-slots (cell-size cell-origin) grid
    (with-vectors ((p (v- point cell-origin)) (s cell-size))
      (let ((px (/ px sx))
            (py (/ py (* sy (y-axis grid)))))
        (vround (vec px py))))))

(defmethod cell-select-line ((grid quad-grid) cell1 cell2)
  "Get a list of cells that intersect a line drawn from one cell to another."
  (loop :with distance = (cell-distance grid cell1 cell2)
        :with step = (/ 1.0 (max distance 1))
        :with start = (cell-nudge cell1)
        :with end = (cell-nudge cell2)
        :for cell :to distance
        :collect (vround (vlerp start end (* step cell)))))

(defmethod cell-select-range ((grid quad-grid) cell range)
  "Get a list of cells up to a certain range of cells away from a cell."
  (loop :for x :from (- range) :to range
        :append (loop :for y :from (- range) :to range
                      :collect (v+ cell (vec x y)))))

(defmethod cell-neighbor-by-index ((grid quad-grid) cell index)
  "Low-level function to get a cell's neighbor given an index."
  (v+ cell (elt (cell-neighbor-offsets grid) index)))
