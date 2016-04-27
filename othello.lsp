(load 'chk.lsp)
(load 'flip.lsp)
(load 'generate_successors.lsp)
(load 'minimax.lsp)
(load 'heuristic.lsp)

(defvar *GAME_BOARD* '(- - - - - - - -
                       - - - - - - - -
                       - - - - - - - -
                       - - - W B - - -
                       - - - B W - - -
                       - - - - - - - -
                       - - - - - - - -
                       - - - - - - - -)
)

(defun reset_brd ()
    "(reset_brd) resets the Othello board"
    (dotimes (i 8)
        (dotimes (j 8)
            (set_pos i j '-)
        )
    )

    (place_piece 3 3 'W)
    (place_piece 3 4 'B)
    (place_piece 4 3 'B)
    (place_piece 4 4 'W)
)

(defun set_pos (row col player)
    "(make_move row col value) changes the value at the row and column to the
    value, this function should not be accessible to user"
    (let (index)
        (setf index (+ (* row 8) col))
        (setf (nth index *GAME_BOARD*) player)
    )

    (eval 't)
)

(defun make-move (position player ply)
    "(make-move position player ply) this will generate a move for the computer
     player, also used in Comp V Comp takes current board position, the color
     to place and the number of levels to go down"
     
    (let (my_move temp_state)
	( setf temp_state ( copy-list *GAME_BOARD* ) )
        ( setf my_move ( caadr ( minimax temp_state ply player 'Max ) ) )
        (place_piece (- (car my_move) 1) (- (cadr my_move) 1) player)
        (format t "Here is my move: ~S ~S~%~%" (car my_move) (cadr my_move))
        (prt_brd *GAME_BOARD*)
        my_move
    )
     
)

(defun set_brd (position)
    "(set_brd position) sets our global game board to the position passed into
     make_move"

)

(defun place_piece (row col player)
    "(place_piece row col value) changes the value at the row and column to the
    value, this function should not be accessible to user"
    (let (index)
        (setf index (+ (* row 8) col))
        (setf (nth index *GAME_BOARD*) player)
        (when (chk_up *GAME_BOARD* player row col)
              (flip_up *GAME_BOARD* player row col)
        )
        (when (chk_down *GAME_BOARD* player row col)
              (flip_down *GAME_BOARD* player row col)
        )
        (when (chk_right *GAME_BOARD* player row col)
              (flip_right *GAME_BOARD* player row col)
        )
        (when (chk_left *GAME_BOARD* player row col)
              (flip_left *GAME_BOARD* player row col)
        )
        (when (chk_ul *GAME_BOARD* player row col)
              (flip_ul *GAME_BOARD* player row col)
        )
        (when (chk_ur *GAME_BOARD* player row col)
              (flip_ur *GAME_BOARD* player row col)
        )
        (when (chk_dr *GAME_BOARD* player row col)
              (flip_dr *GAME_BOARD* player row col)
        )
        (when (chk_dl *GAME_BOARD* player row col)
              (flip_dl *GAME_BOARD* player row col)
        )
    )

    (eval 't)
)

(defun move (player)
    "(move player) takes a move for a player (called player) if their move is
     valid we change the board and true is replayered else we replayer NIL"
    (format t "What is your move [row col]? ")
    (let (row col valid)
        (setf row (read))
        (setf col (read))
        (cond
            ((check_move *GAME_BOARD* row col player) 
                         (place_piece (- row 1) (- col 1) player)
             (setf valid 't)
            )
            (t NIL)
        )
        (prt_brd *GAME_BOARD*)

        (when (not valid) (format t "Invalid move try again~%"))
        (eval valid) ; make loop based on valid
    )
)

(defun prt_brd (board)
    "(prt_brd board) prints the given Othello board"
    (format t " ")
    (dotimes (i 8)
       (format t " ~S" (+ 1 i))
    )
    (format t "~%")

    (let (rowc index)
        (setf rowc 1)
        (dotimes (row 8)
            (format t "~S " rowc)
            (dotimes (col 8)
                (setf index (+ (* row 8) col))
                (format t "~S " (nth index board))
            )
            (format t "~%")
            (incf rowc)
        )
    )
)

(defun count_pieces (color)
    "(count_pieces color) will count the colored pieces on the board at end"

    (let ((count 0) index)
        (dotimes (i 8)
            (dotimes (j 8)
                (setf index (+ (* i 8) j))
                (when (equal color (nth index *GAME_BOARD*)) (incf count))
            )
        )
        (eval count) ; return the count
    )
)

(defun othello-init ( )
    "(othello-init) will reset the global game board"
    (reset_brd)
)

(defun othello (&optional player)
    "(othello [player]) will prompt player if they want to go first if black or
     white wasn't specified then pits man vs. machine, like John Henry"
    (let ((again T) yes_or_no human w_score b_score)
        (loop while again do
        (cond
            ((null player)
                (loop while (and (not (equalp yes_or_no 'Y))
                                (not (equalp yes_or_no 'N))) do
                    (format t "Would you like to go first [y/n]: ")
                    (setf yes_or_no (read))

                    (when (equalp yes_or_no 'YES) (setf yes_or_no 'Y))
                    (when (equalp yes_or_no 'NO) (setf yes_or_no 'N))

                    (if (or (equalp yes_or_no 'Y) (equalp yes_or_no 'YES))
                        ; set player color
                        (setf human 'B)
                        (setf human 'W)
                    )
                )
            ) ; end the null player cond
                (t (if (equal player 'black) (setf human 'B) (setf human 'W)))
        ) ; get player first preference if color wasn't specified or sets
          ; human color

        (othello-init)
        (prt_brd *GAME_BOARD*) ; print the game board for the start 

        (loop while (or (generate_successors *GAME_BOARD* 'B)
                        (generate_successors *GAME_BOARD* 'W)) do
            (cond
                ; loop if an invalid move is made
                ((equal human 'B)
                    (when (generate_successors *GAME_BOARD* 'B)
                        (loop while (not (move 'B))))
                    (when (generate_successors *GAME_BOARD* 'W)
                          (make-move *GAME_BOARD* 'W 7))
                )
                (t 
                    (when (generate_successors *GAME_BOARD* 'B)
                          (make-move *GAME_BOARD* 'B 7))
                    (when (generate_successors *GAME_BOARD* 'W) 
                        (loop while (not (move 'W))))
                )
            )
        );loop till no moves can be generated

        (setf b_score (count_pieces 'B))
        (setf w_score (count_pieces 'W))

        (cond
            ((> b_score w_score) (format t "Black won! Well played~%"))
            ((= b_score w_score) (format t "Tie Game! Well played~%"))
            (t (format t "White won! Well played!~%"))
        )

        (format t "White score = ~S~%" w_score)
        (format t "Black score = ~S~%" b_score)

        (setf yes_or_no NIL)
        (loop while (and (not (equalp yes_or_no 'Y))
                                (not (equalp yes_or_no 'N))) do
                    (format t "Would you like to play again [y/n]: ")
                    (setf yes_or_no (read))

                    (when (equalp yes_or_no 'YES) (setf yes_or_no 'Y))
                    (when (equalp yes_or_no 'NO) (setf yes_or_no 'N))

                    (if (or (equalp yes_or_no 'Y) (equalp yes_or_no 'YES))
                        ; set player color
                        (and (setf again t) (setf player NIL))
                        (setf again NIL)
                    )
        );prompt for play again
        )
    ); end let
)
