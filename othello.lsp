(load 'chk.lsp)
(load 'flip.lsp)

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
            (set_pos i j '-)
        )
    )

    (make_move 3 3 'W)
    (make_move 3 4 'B)
    (make_move 4 3 'B)
    (make_move 4 4 'W)
)

(defun set_pos (row col player)
    "(make_move row col value) changes the value at the row and column to the
    value, this function should not be accessible to user"
    (let (curr_row)
        (setf curr_row (nth row *GAME_BOARD*))
        (setf (nth col curr_row) player)
    )

    (eval 't)
)

(defun make_move (row col player)
    "(make_move row col value) changes the value at the row and column to the
    value, this function should not be accessible to user"
    (let (curr_row)
        (setf curr_row (nth row *GAME_BOARD*))
        (setf (nth col curr_row) player)
        (if (chk_up player row col) (flip_up player row col))
	(if (chk_down player row col) (flip_down player row col))
        (if (chk_right player row col) (flip_right player row col))
        (if (chk_left player row col) (flip_left player row col))
        (if (chk_ul player row col) (flip_ul player row col))
        (if (chk_ur player row col) (flip_ur player row col))
        (if (chk_dr player row col) (flip_dr player row col))
        (if (chk_dl player row col) (flip_dl player row col))
    )

    (eval 't)
)

(defun move (player)
        "(move player) takes a move for a player (called player) if their move is
         valid we change the board and true is replayered else we replayer NIL"
        (prt_brd)
        (format t "Please enter a move [row col]: ")
        (let (row col)
           (setf row (read))
           (setf col (read))
           (cond
               ((check_move row col player) (make_move (- row 1) 
                                                  (- col 1) player))
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

(defun othello ( )
    ; we should get the player's color then start rotating till board full
)
