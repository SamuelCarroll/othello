#|
                  ***** MINIMAX.LSP *****

Generalized recursive minimax routine.

Author: Dr. John M. Weiss
Modified by: Leif Torgersen and Samuel Carroll
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (minimax position depth)
          where position is the position to be evaluated,
          and depth is the search depth (number of plys).

Returns:  (value path)
          where value is the backed-up value from evaluation of leaf nodes,
          and path is the path to the desired leaf node.

Functions called:

          (deepenough depth) -
              predicate that returns T if the current position has reached
              the desired search depth, NIL otherwise.

          (move-generator position) -
              generates successors to the position.

          (static position) -
              applies the static evaluation function to the position.

          Note: these functions may need additional arguments.
|#

(defun minimax (position depth player Max_or_Min alpha beta)
	"(minimax position depth player Max_or_Min alpha beta) runs minimax
	 with Alpha-Beta Pruning to find a move, needs depth to search whos
	 turn it is if they are Max or Min and the above alpha and beta scores"
	; if we have searched deep enough, or there are no successors,
	; return position evaluation and nil for the path
	(if (or (equal depth 0) (null (generate_successors position player)))
		(list (heuristic position player) nil)

		; otherwise, generate successors and run minimax recursively
		(let
			(
				; generate list of sucessor positions
				(successors (generate_successors position player))

				; initialize current best path to nil
				(best-path nil)

				; initialize current best score to negative infinity
				(best-score -1000000)

				; other local variables
				succ-value
				succ-score
				not_player
			)

			(if (equalp player 'B) (setf not_player 'W) (setf not_player 'B))
			; explore possible moves by looping through successor positions
			(dolist (successor successors)

				; perform recursive DFS exploration of game tree
				
				(cond ((null succ-value)
					(setf succ-value (minimax (car successor) (1- depth) not_player 'Min alpha beta)))
					(t
						(cond  
							((equalp Max_or_Min 'Max)
								(when (< alpha beta)
									(setf succ-value (minimax (car successor) (1- depth) not_player 'Min alpha beta))
									(setf alpha (- (car succ-value)))
								)
							)
							((equalp Max_or_Min 'Min)
								(when (< beta alpha)
									(setf succ-value (minimax (car successor) (1- depth) not_player 'Max alpha beta))
									(setf beta (- (car succ-value)))
								)
							)
						)
					)
				)


				; change sign every ply to reflect alternating selection
				; of MAX/MIN player (maximum/minimum value)
				(setf succ-score (- (car succ-value)))

				; update best value and path if a better move is found
				; (note that path is being stored in reverse order)
				(when (> succ-score best-score)
					(setf best-score succ-score)
					(setf best-path (cons (cadr successor) (cdr succ-value)))
				)
			)

			; return (value path) list when done
			(list best-score best-path)
		)
	)
)
