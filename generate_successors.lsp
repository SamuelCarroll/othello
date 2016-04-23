(defun generate_successors ( player )
	"(generate_successors player) Creats a list of all legal moves as a
	list of 2 elements, a row position and a column p[osition"
	( let ( succ move )

		( dotimes ( i 8 )
			
			( dotimes ( j 8 )

				( when ( check_move ( 1+ i ) ( 1+ j ) player )
					( setf move ( list (+ 1 i) (+ 1 j) ) )
					( setf succ ( cons move succ ) )
				)

			)

		)

		succ
	)

)
