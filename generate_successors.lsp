#|
                    ***** generate_successors.lsp *****

This uses the flip code to generate successors for a given,
state of the board.

Authors: Leif Torgersen
Written Spring 2016 for CSC447/547 AI class.

|#

(defun generate_successors ( position player )
	"(generate_successors position player) Creats a list of all legal moves as a
	list of 2 elements, a row position and a column p[osition"
	( let ( succ row col index temp_position )
		( dotimes ( i 8 )
	
			( dotimes ( j 8 )

				( setf temp_position ( copy-list position ) )

				( when ( check_move temp_position ( 1+ i ) ( 1+ j ) player )
					( setf row i )
					( setf col j )
					(setf index (+ (* row 8) col))
					(setf (nth index temp_position) player)
					(when (chk_up temp_position player row col)
						  (flip_up temp_position player row col)
		  			)
		  			(when (chk_down temp_position player row col)
						  (flip_down temp_position player row col)
		   			)
	  				(when (chk_right temp_position player row col)
	  					  (flip_right temp_position player row col)
					)
					(when (chk_left temp_position player row col)
						  (flip_left temp_position player row col)
					)
					(when (chk_ul temp_position player row col)
						  (flip_ul temp_position player row col)
					)
					(when (chk_ur temp_position player row col)
						  (flip_ur temp_position player row col)
					)
					(when (chk_dr temp_position player row col)
						  (flip_dr temp_position player row col)
					)
					(when (chk_dl temp_position player row col)
						  (flip_dl temp_position player row col)
					)
					( setf succ ( cons (list temp_position (list (+ 1 i) (+ 1 j))) succ ) )
				)

			)

		)
		succ
	)

)
