(defun chk_empty (row col)
    "(chk_empty row col) checks if the move is in an empty space"
    (let (index elem valid)
        (setf index (+ (* row 8) col))
        (setf elem (nth index *GAME_BOARD*))
        (cond
            ((equal elem '-) (setf valid t))
            (t NIL)
        )
        (eval valid)
    )  
)

(defun check_move (row col player)
    "(check_move move player) checks if a move of form (row column) is valid
     for the specified player, returning true if it is"
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

(defun chk_up (player row col)
    "(chk_up player row col) checks if the move is valid going up"
    (let (elem index valid)
        (setf index (+ (* (- row 1) 8) col))
        (dotimes (i (- 8 (- 8 row)))
            (setf elem (nth index *GAME_BOARD*))
            (cond
                ((equal elem player) (return-from chk_up valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf valid t))
            )
            (setf index (- index 8))
        )
    )
)

(defun chk_down (player row col)
    "(chk_down player row col) check if the move is valid going down"
    (let (valid index elem)
        (setf index (+ (* (+ row 1) 8) col))
        (dotimes (i (- 7 row))
            (setf elem (nth index *GAME_BOARD*))
            (cond
                ((equal elem player) (return-from chk_down valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf valid t))
            )
            (setf index (+ index 8))
        )
    )
)

(defun chk_right (player row col)
    "(chk_right player row col) check if the move is valid going right"
    (let (valid index elem)
        (setf index (+ (* row 8) (+ 1 col)))
        (dotimes (i (- 7 col))
            (setf elem (nth index *GAME_BOARD*))
            (cond
                ((equal elem player) (return-from chk_right valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf valid t))
            )
            (setf index (+ index 1))
        )
    )
)

(defun chk_left (player row col)
    "(chk_left player row col) checks if the move is valid going left"
    (let (valid index elem)
        (setf index (+ (* row 8) (- col 1)))
        (dotimes (i (- 8 (- 8 col)))
            (setf elem (nth index *GAME_BOARD*))
            (cond
                ((equal elem player) (return-from chk_left valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf valid t))
            )
            (setf index (- index 1))
        )
    )
)

(defun chk_ul (player row col)
    "(chk_ul player row col) checks if the move is valid going up and left"
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
                ((equal elem player) (return-from chk_ul valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf valid t))
            )
            (setf index (- index 9))
        )
    )
    
)

(defun chk_ur (player row col)
    "(chk_ur player row col) checks if the move is valid going up and right"
    (let (valid index elem above right limit)
        (setf above (- 8 (- 8 row)))
        (setf right (- 7 col))
        (cond
            ((< above right) (setf limit above))
            ((< right above) (setf limit right))
            (t (setf limit right))
        )
        (setf index (+ (* (- row 1) 8) (+ col 1)))
        (dotimes (i limit)
            (setf elem (nth index *GAME_BOARD*))
            (cond
                ((equal elem player) (return-from chk_ur valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf valid t))
            )
            (setf index (- index 7))
        )
    )
)

(defun chk_dr (player row col)
    "(chk_dr player row col) checks if the move is valid going down and right"
    (let (valid index elem below right limit)
        (setf below (- 7 row))
        (setf right (- 7 col))
        (cond
            ((< below right) (setf limit below))
            ((< right below) (setf limit right))
            (t (setf limit right))
        )
        (setf index (+ (* (+ row 1) 8) (+ col 1)))
        (dotimes (i limit)
            (setf elem (nth index *GAME_BOARD*))
            (cond
                ((equal elem player) (return-from chk_dr valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf valid t))
            )
            (setf index (+ index 9))
        )
    )
)

(defun chk_dl (player row col)
    "(chk_dl player row col) checks if the move is valid going down and left"
    (let (valid index elem below left limit)
        (setf below (- 7 row))
        (setf left (- 8 (- 8 col)))
        (cond
            ((< below left) (setf limit below))
            ((< left below) (setf limit left))
            (t (setf limit left))
        )
        (setf index (+ (* (+ row 1) 8) (- col 1)))
        (dotimes (i limit)
            (setf elem (nth index *GAME_BOARD*))
            (cond
                ((equal elem player) (return-from chk_dl valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf valid t))
            )
            (setf index (+ index 7))
        )
    )
)
