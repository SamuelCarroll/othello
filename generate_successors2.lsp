(defun generate_successors ( color )

	( let ( succ move )

		( dotimes ( i 8 )
			
			( dotimes ( j 8 )

				( when ( check_move ( 1+ i ) ( 1+ j ) color )
					( setf move ( list i j ) )
					( cons move succ )
				)

			)

		)

		succ
	)

)
