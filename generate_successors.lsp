(defun generate_successors (state color)

	( let ( ( succ '() ) move_position ( row_position 0 ) )

		( dolist ( row state )
			
			( dolist ( col row )

				( when ( and ( not ( equal col color ) ) ( not ( null col ) ) )

					( setf move_position ( position col state ) )

					( when ( null ( nth ( - move_position 1 ) row ) )

						;Check if it will flip a tile
						;Check this by making the play, flipping the tiles,
						;and seeing if more than one tile was added to a color

					)

					( when ( null ( nth ( + move_position 1 ) row ) )

					

					)

					( when ( null ( nth move_position ( nth row_position state ) ) )

					

					)

					( when ( null ( nth move_position  ( nth row_position state ) ) )

					

					)

					( when ( null ( nth ( - move_position 1 )  ( nth ( + row_position 1 ) state ) ) )

					

					)

					( when ( null ( nth ( + move_position 1 )  ( nth ( + row_position 1 ) state ) ) )

					

					)

					( when ( null ( nth ( + move_position 1 )  ( nth ( - row_position 1 ) state ) ) )

					

					)

					( when ( null ( nth ( - move_position 1 )  ( nth ( - row_position 1 ) state ) ) )

					

					)

				)

				( setf row_position ( 1+ row_position ) )

			)

		)

	)

)
