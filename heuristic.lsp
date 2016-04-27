#|
                    ***** heuristic.lsp *****

This is the code to check the strength of a play, doesn't seem to work
great but need to turn this in. 

Authors: Leif Torgersen
Written Spring 2016 for CSC447/547 AI class.

|#

(load 'generate_successors.lsp)

(defun heuristic (state player)
        "(heuristic state player) evaluates a given state and assigns a value
        to how good of a move it is"
	(let (
		(value 0) 
		not_player 
		(min_val 0) 
		(max_val 0) 
		(position_stability 0) 
		(weight_list '(120 -20 20 5 5 20 -20 120
				 -20 -40 -5 -5 -5 -5 -40 -20
				 20 -5 15 3 3 15 -5 20
				 5 -5 3 3 3 3 -5 5
				 5 -5 3 3 3 3 -5 5
				 20 -5 15 3 3 15 -5 20
				 -20 -40 -5 -5 -5 -5 -40 -20
				 120 -20 20  5  5 20 -20 120)
		))
		;Sets what the other player is to compare state values
		(when (equal player 'B) (setf not_player 'W))
		(when (equal player 'W) (setf not_player 'B))

		;Checks corner count for each player and factors it into value
		(when (equal (nth 0 state) player)
			(incf max_val)
		)

		(when (equal (nth 7 state) player)
			(incf max_val)
		)

		(when (equal (nth 56 state) player)
			(incf max_val)
		)

		(when (equal (nth 63 state) player)
			(incf max_val)
		)

		(when (equal (nth 0 state) not_player)
			(incf min_val)
		)

		(when (equal (nth 7 state) not_player)
			(incf min_val)
		)

		(when (equal (nth 56 state) not_player)
			(incf min_val)
		)

		(when (equal (nth 63 state) not_player)
			(incf min_val)
		)
		(setf max_val (+ max_val .0000000000000001))
		(when (not (= (+ max_val min_val) 0))
			(setf value (+ value (* 100 (/ (- max_val min_val) (+ max_val min_val)))))
		)
		

		;checks the mobility for each player (number of posible moves) and factors it into value
		(setf max_val (+ (length (generate_successors state player)) .0000000000000001))
		(setf min_val (length (generate_successors state not_player)))
		(when (not (= (+ max_val min_val) 0))
			(setf value (+ value (* 100 (/ (- max_val min_val) (+ max_val min_val)))))
		)

		;checks coin parity and factors it into value
		(setf max_val (+ (count_pieces player) .0000000000000001))
		(setf min_val (count_pieces not_player))
		(when (not (= (+ max_val min_val) 0))
			(setf value (+ value (* 100 (/ (- max_val min_val) (+ max_val min_val)))))
		)

		;checks stability of each player's pieces and factors it into value
		(setf max_val 0)
		(setf min_val 0)
		(dolist (place state)
			(when (equal place player)
				(setf max_val (+ max_val (nth position_stability weight_list)))
			)
			(when (equal place not_player)
				(setf min_val (+ min_val (nth position_stability weight_list)))
			)
			(incf position_stability)
		)
		(setf max_val (+ (count_pieces player) .0000000000000001))
		(when (not (= (+ max_val min_val) 0))
			(setf value (+ value (* 100 (/ (- max_val min_val) (+ max_val min_val)))))
		)
		(eval value)
	)
)
