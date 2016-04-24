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
     
    (let ()
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
;         (when (chk_up player row col) (flip_up player row col))
 ;        (when (chk_down player row col) (flip_down player row col))
  ;       (when (chk_right player row col) (flip_right player row col))
   ;      (when (chk_left player row col) (flip_left player row col))
    ;     (when (chk_ul player row col) (flip_ul player row col))
     ;    (when (chk_ur player row col) (flip_ur player row col))
      ;   (when (chk_dr player row col) (flip_dr player row col))
       ;  (when (chk_dl player row col) (flip_dl player row col))
    )

    (eval 't)
)

(defun move (player)
        "(move player) takes a move for a player (called player) if their move is
         valid we change the board and true is replayered else we replayer NIL"
        (format t "Please enter a move [row col]: ")
        (let (row col)
           (setf row (read))
           (setf col (read))
           (cond
               ((check_move row col player) (place_piece (- row 1) 
                                                  (- col 1) player))
               (t NIL)
           )
        )
        (prt_brd *GAME_BOARD*)
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
                (setf index (+ (i * 8) j))
                (when (equal color (nth index *GAME_BOARD*)) (incf count))
            )
        )
        (eval count) ; return the count
    )
)

(defun othello (&optional player)
    "(othello [player]) will prompt player if they want to go first if black or
     white wasn't specified then pits man vs. machine, like John Henry"
    (let (yes_or_no human w_score b_score)
        ; TODO loop user input while not valid
        (cond
            ((null player)
                (format t "Would you like to go first [y/n]: ")
                (setf yes_or_no (read))
                (if (or (equalp yes_or_no 'Y) (equalp yes_or_no 'YES))
                    (setf human 'B) ; set human to black if player selects first
                    (setf human 'W) ; set human to white if player selects not first
                )
            )
            (t (if (equal player 'black) (setf human 'B) (setf human 'W)))
        ) ; get player first preference if color wasn't specified or sets human color

        (prt_brd) ; print the game board for the start 

        (loop while (or (generate_successors 'B) (generate_successors 'W)) do
            (cond
                ((equal human 'B)
                    (when (generate_successors 'B) (move 'B))
                    (when (generate_successors 'W) (make-move *GAME_BOARD* 'W 4))
                )
                (t 
                    (when (generate_successors 'B) (make-move *GAME_BOARD* 'B 4))
                    (when (generate_successors 'W) (move 'W))
                )
            )
        );loop till no moves can be generated

        (setf b_score (count_pieces 'B))
        (setf w_score (count_pieces 'W))

        (cond
            ((> b_score w_score) (format t "Black won! Well played~%"))
            (t (format t "White won! Well played!~%"))
        )

        ;prompt for play again
    ); end let
)
