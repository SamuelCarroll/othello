(defun flip_up (turn row col)
    "(flip_up turn row col) checks if the move is valid going up"
    (let (valid curr_row elem)
        (dotimes (i (- 8 (- 8 row)))
            (setf curr_row (nth (- row (+ 1 i)) *GAME_BOARD*))
            (setf elem (nth col curr_row))
            (cond
                ((equal elem turn) (return-from flip_up valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf (nth col curr_row) turn) (prt_brd))
            )
        )
    )
)

(defun flip_down (turn row col)
    "(flip_down turn row col) check if the move is valid going down"
    (let (valid curr_row elem)
        (setf curr_row row)
        (dotimes (i (- 7 row))
            (setf curr_row (nth (+ (+ 1 i) row) *GAME_BOARD*))
            (setf elem (nth col curr_row))
            (cond
                ((equal elem turn) (return-from flip_down valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf valid t))
            )
        )
    )
)

(defun flip_right (turn row col)
    "(flip_right turn row col) check if the move is valid going right"
    (let (valid curr_row elem)
        (setf valid NIL)
        (setf curr_row row)
        (dotimes (i (- 7 col))
            (setf curr_row (nth row *GAME_BOARD*))
            (setf elem (nth (+ (+ 1 i) col) curr_row))
            (cond
                ((equal elem turn) (return-from flip_right valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf valid t))
            )
        )
    )
)

(defun flip_left (turn row col)
    "(flip_left turn row col) checks if the move is valid going left"
    (let (valid curr_row elem)
        (dotimes (i (- 8 (- 8 col)))
            (setf curr_row (nth row *GAME_BOARD*))
            (setf elem (nth (- col (+ 1 i)) curr_row))
            (cond
                ((equal elem turn) (return-from flip_left valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf valid t))
            )
        )
    )
)

(defun flip_ul (turn row col)
    "(flip_ul turn row col) checks if the move is valid going up and left"
    (let (valid curr_row elem above left limit)
        (setf above (- 8 (- 8 row)))
        (setf left (- 8 (- 8 col)))
        (cond
            ((< above left) (setf limit above))
            ((< left above) (setf limit left))
            (t (setf limit left))
        )
        (dotimes (i limit)
            (setf curr_row (nth (- row (+ 1 i)) *GAME_BOARD*))
            (setf elem (nth (- col (+ 1 i)) curr_row))
            (cond
                ((equal elem turn) (return-from flip_ul valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf valid t))
            )
        )
    )
)

(defun flip_ur (turn row col)
    "(flip_ur turn row col) checks if the move is valid going up and right"
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
                (t (setf valid t))
            )
        )
    )
)

(defun flip_dr (turn row col)
    "(flip_dr turn row col) checks if the move is valid going down and right"
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
                (t (setf valid t))
            )
        )
    )
)

(defun flip_dl (turn row col)
    "(flip_dl turn row col) checks if the move is valid going down and left"
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
                (t (setf valid t))
            )
        )
    )
)
