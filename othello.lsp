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
)

(defun move (turn)
        (prt_brd)
        (format t "Please enter a move (row col): ")
        (let (move)
           (setf move (read))
           ;(check_move move)
           (make_move (- (car move) 1) (- (cadr move) 1) turn)
        )
        (format t "~%") 
)

;(defun check_move (move))

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
 
