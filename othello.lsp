(defvar *GAME_BOARD* '((- - - - - - - -)
                       (- - - - - - - -)
                       (- - - - - - - -)
                       (- - - W B - - -)
                       (- - - B W - - -)
                       (- - - - - - - -)
                       (- - - - - - - -)
                       (- - - - - - - -))
)

(defun reset_brd ()
    "(reset_brd) resets the Othello board"
    (dotimes (i 8)
        (dotimes (j 8)
            (make_move i j '-)
        )
    )

    (make_move 3 3 'W)
    (make_move 3 4 'B)
    (make_move 4 3 'B)
    (make_move 4 4 'W)
)

(defun make_move (row col value)
    "(make_move row col value) changes the value at the row and column to the
    value, this function should not be accessible to user"
    (let (curr_row)
        (setf curr_row (nth row *GAME_BOARD*))
        (setf (nth col curr_row) value)
    )

    (eval 't)
)

(defun move (turn)
        (prt_brd)
        (format t "Please enter a move (row col): ")
        (let (move)
           (setf move (read))
           (cond
               ((check_move move turn) (make_move (- (car move) 1) 
                                                  (- (cadr move) 1) turn))
               (t NIL)
           )
        )
)

(defun check_move (move turn)
    (let (row col valid)
        (setf row (car move))
        (setf col (cadr move))
        (decf row)
        (decf col)
        (cond 
            ((chk_up turn row col) (setf valid t))
            ((chk_down turn row col) (setf valid t))
            ((chk_right turn row col) (setf valid t))
  ;          ((chk_left turn row col) (setf valid t))
   ;         ((chk_nw turn row col) (setf valid t))
    ;        ((chk_ne turn row col) (setf valid t))
     ;       ((chk_se turn row col) (setf valid t))
      ;      ((chk_sw turn row col) (setf valid t))
            (t NIL)
        )
    )
)

(defun chk_up (turn row col)
    (let (valid curr_row elem)
        (dotimes (i (- 7 (- 8 row))) 
            (setf curr_row (nth (- row i) *GAME_BOARD*))
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
    (let (valid curr_row elem)
        (setf curr_row row)
        (dotimes (i (- 8 row))
            (setf curr_row (nth (+ i row) *GAME_BOARD*))
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
    (let (valid curr_row elem)
        (setf valid NIL)
        (setf curr_row row)
        (dotimes (i (- 8 col))
            (setf curr_row (nth row *GAME_BOARD*))
            (setf elem (nth (+ i col) curr_row))
            (cond 
                ((equal elem turn) (return-from chk_right valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf valid t))
            )
        )
    )
)

(defun chk_left (turn row col)
    (let (valid curr_row elem)
        (dotimes (i (- 8 (- 8 col)))
            (setf curr_row (nth row *GAME_BOARD*))
            (setf elem (nth (- col i) curr_row))
            (cond
                ((equal elem turn) (return-from chk_left valid))
                ((equal elem '-) (setf i 8)) ; force a break
                (t (setf valid t))
            )
        )
    )
)


;(defun chk_nw (turn row col)

;(defun chk_ne (turn row col)

;(defun chk_se (turn row col)

;(defun chk_sw (turn row col)

(defun prt_brd ()
    "(prt_brd) prints the current Othello board"

    (format t " ")
    (dotimes (i 8)
       (format t " ~S" (+ 1 i))
    )
    (format t "~%")

    (let (rowc)
        (setf rowc 1)
        (dolist (row *GAME_BOARD*)
            (format t "~S " rowc)
            (dolist (elem row)
                (format t "~S " elem)
            )
            (format t "~%")
            (incf rowc)
        )
    )
)
 
