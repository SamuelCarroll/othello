(defun chk_empty (row col)
    "(chk_empty row col) checks if the move is in an empty space"
    (let (valid curr_row elem)
        (setf curr_row (nth row *GAME_BOARD*))
        (setf elem (nth col curr_row))
        (cond
            ((equal elem '-) (setf valid t))
            (t NIL)
        )
    )
)

(defun check_move (row col player)
    "(check_move move player) checks if a move of form (row column) is valid
     for the specified player, replayering true if it is"
    (let (valid)
        (decf row)
        (decf col)
        (cond
            ((chk_empty row col)
                (cond
                    ((chk_up player row col) (setf valid t))
                    ((chk_down player row col) (setf valid t))
                    ((chk_right player row col) (setf valid t))
                    ((chk_left player row col) (setf valid t))
                    ((chk_ul player row col) (setf valid t))
                    ((chk_ur player row col) (setf valid t))
                    ((chk_dr player row col) (setf valid t))
                    ((chk_dl player row col) (setf valid t))
                    (t NIL)
                )
            ); don't check if we don't place a piece on a non-empty position
            (t NIL) ; if everything else fails return false
        )
    )
)

(defun chk_up (turn row col)
    "(chk_up turn row col) checks if the move is valid going up"
    (let (valid curr_row elem)
        (dotimes (i (- 8 (- 8 row)))
            (setf curr_row (nth (- row (+ 1 i)) *GAME_BOARD*))
            (setf elem (nth col curr_row))
            (cond
                ((equal elem turn) (return-from chk_up valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf valid t))
            )
        )
    )
)

(defun chk_down (turn row col)
    "(chk_down turn row col) check if the move is valid going down"
    (let (valid curr_row elem)
        (setf curr_row row)
        (dotimes (i (- 7 row))
            (setf curr_row (nth (+ (+ 1 i) row) *GAME_BOARD*))
            (setf elem (nth col curr_row))
            (cond
                ((equal elem turn) (return-from chk_down valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf valid t))
            )
        )
    )
)

(defun chk_right (turn row col)
    "(chk_right turn row col) check if the move is valid going right"
    (let (valid curr_row elem)
        (setf valid NIL)
        (setf curr_row row)
        (dotimes (i (- 7 col))
            (setf curr_row (nth row *GAME_BOARD*))
            (setf elem (nth (+ (+ 1 i) col) curr_row))
            (cond
                ((equal elem turn) (return-from chk_right valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf valid t))
            )
        )
    )
)

(defun chk_left (turn row col)
    "(chk_left turn row col) checks if the move is valid going left"
    (let (valid curr_row elem)
        (dotimes (i (- 8 (- 8 col)))
            (setf curr_row (nth row *GAME_BOARD*))
            (setf elem (nth (- col (+ 1 i)) curr_row))
            (cond
                ((equal elem turn) (return-from chk_left valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf valid t))
            )
        )
    )
)

(defun chk_ul (turn row col)
    "(chk_ul turn row col) checks if the move is valid going up and left"
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
                ((equal elem turn) (return-from chk_ul valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf valid t))
            )
        )
    )
)

(defun chk_ur (turn row col)
    "(chk_ur turn row col) checks if the move is valid going up and right"
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
                ((equal elem turn) (return-from chk_ur valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf valid t))
            )
        )
    )
)

(defun chk_dr (turn row col)
    "(chk_dr turn row col) checks if the move is valid going down and right"
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
                ((equal elem turn) (return-from chk_dr valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf valid t))
            )
        )
    )
)

(defun chk_dl (turn row col)
    "(chk_dl turn row col) checks if the move is valid going down and left"
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
                ((equal elem turn) (return-from chk_dl valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf valid t))
            )
        )
    )
)
