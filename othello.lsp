(load chk.lsp)

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
        "(move turn) takes a move for a player (called turn) if their move is
         valid we change the board and true is returned else we return NIL"
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
    "(check_move move turn) checks if a move of form (row column) is valid
     for the specified player called turn, returning true if it is"
    (let (row col valid)
        (setf row (car move))
        (setf col (cadr move))
        (decf row)
        (decf col)
        (cond 
            ((chk_up turn row col) (setf valid t))
            ((chk_down turn row col) (setf valid t))
            ((chk_right turn row col) (setf valid t))
            ((chk_left turn row col) (setf valid t))
            ((chk_ul turn row col) (setf valid t))
            ((chk_ur turn row col) (setf valid t))
            ((chk_dr turn row col) (setf valid t))
            ((chk_dl turn row col) (setf valid t))
            (t NIL)
        )
    )
)

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
