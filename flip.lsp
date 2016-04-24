(defun flip_up (turn row col)
    "(flip_up turn row col) flips pieces going up"
    (let (valid index elem)
        (setf index (+ (* (- row 1) 8) col))
        (dotimes (i (- 8 (- 8 row)))
            (setf elem (nth index *GAME_BOARD*))
            (cond
                ((equal elem turn) (return-from flip_up valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf (nth index *GAME_BOARD*) turn))
            )
            (setf index (- index 8))
        )
    )
)

(defun flip_down (turn row col)
    "(flip_down turn row col) flips pieces going down"
    (let (valid index elem)
        (setf index (+ (* (+ row 1) 8) col))
        (dotimes (i (- 7 row))
            (setf elem (nth index *GAME_BOARD*))
            (cond
                ((equal elem turn) (return-from flip_down valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf (nth index *GAME_BOARD*) turn))
            )
            (setf index (+ index 8))
        )
    )
)

(defun flip_right (turn row col)
    "(flip_right turn row col) flips pieces going right"
    (let (valid index elem)
        (setf index (+ (* row 8) (+ 1 col)))
        (dotimes (i (- 7 col))
            (setf elem (nth index *GAME_BOARD*))
            (cond
                ((equal elem turn) (return-from flip_right valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf (nth index *GAME_BOARD*) turn))
            )
            (setf index (+ index 1))
        )
    )
)

(defun flip_left (turn row col)
    "(flip_left turn row col) flips pieces going left"
    (let (valid index elem)
        (setf index (+ (* row 8) (- col 1)))
        (dotimes (i (- 8 (- 8 col)))
            (setf elem (nth index *GAME_BOARD*))
            (cond
                ((equal elem turn) (return-from flip_left valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf (nth index *GAME_BOARD*) turn))
            )
        )
    )
)

(defun flip_ul (turn row col)
    "(flip_ul turn row col) flips pieces going up and left"
    (let (valid index elem above left limit)
        (setf above (- 8 (- 8 row)))
        (setf left (- 8 (- 8 col)))
        (cond
            ((< above left) (setf limit above))
            ((< left above) (setf limit left))
            (t (setf limit left))
        )
        (setf index (+ (* (- row 1) 8) (- col 1)))
        (dotimes (i limit)
            (setf elem (nth index *GAME_BOARD*))
            (cond
                ((equal elem turn) (return-from flip_ul valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf (nth index *GAME_BOARD*) turn))
            )
            (setf index (- index 9))
        )
    )
)

(defun flip_ur (turn row col)
    "(flip_ur turn row col) flips pieces going up and right"
    (let (valid curr_row elem above right limit)
        (setf above (- 8 (- 8 row)))
        (setf right (- 7 col))
        (cond
            ((< above right) (setf limit above))
            ((< right above) (setf limit right))
            (t (setf limit right))
        )
        (dotimes (i limit)
            (setf curr_row (nth (- row (+ 1 i)) *GAME_BOARD*))
            (setf elem (nth (+ col (+ 1 i)) curr_row))
            (cond
                ((equal elem turn) (return-from flip_ur valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf (nth (+ col (+ 1 i)) curr_row) turn))
            )
        )
    )
)

(defun flip_dr (turn row col)
    "(flip_dr turn row col) flips pieces going down and right"
    (let (valid curr_row elem below right limit)
        (setf below (- 7 row))
        (setf right (- 7 col))
        (cond
            ((< below right) (setf limit below))
            ((< right below) (setf limit right))
            (t (setf limit right))
        )
        (dotimes (i limit)
            (setf curr_row (nth (+ row (+ 1 i)) *GAME_BOARD*))
            (setf elem (nth (+ col (+ 1 i)) curr_row))
            (cond
                ((equal elem turn) (return-from flip_dr valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf (nth (+ col (+ 1 i)) curr_row) turn))
            )
        )
    )
)

(defun flip_dl (turn row col)
    "(flip_dl turn row col) flips pieces going down and left"
    (let (valid curr_row elem below left limit)
        (setf below (- 7 row))
        (setf left (- 8 (- 8 col)))
        (cond
            ((< below left) (setf limit below))
            ((< left below) (setf limit left))
            (t (setf limit left))
        )
        (dotimes (i limit)
            (setf curr_row (nth (+ row (+ 1 i)) *GAME_BOARD*))
            (setf elem (nth (- col (+ 1 i)) curr_row))
            (cond
                ((equal elem turn) (return-from flip_dl valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf (nth (- col (+ 1 i)) curr_row) turn))
            )
        )
    )
)
