(defun generate_successors (state color)

	( let ( ( succ '() ) move_position )

		( dolist ( row state )
			
			( dolist ( col row )

				( when ( and ( not ( equal col color ) ) ( not ( null col ) ) )

					( setf move_position ( position col state ) )

					( when ( null ( nth ( - move_position 1 ) state ) )

						;Check if it will flip a tile
						;Check this by making the play, flipping the tiles,
						;and seeing if more than one tile was added to a color

					)

					( when ( null ( nth ( + move_position 1 ) state ) )

					

					)

					( when ( null ( nth ( - move_position 8 ) state ) )

					

					)

					( when ( null ( nth ( + move_position 8 ) state ) )

					

					)

					( when ( null ( nth ( + move_position 7 ) state ) )

					

					)

					( when ( null ( nth ( + move_position 9 ) state ) )

					

					)

					( when ( null ( nth (  move_position 7 ) state ) )

					

					)

					( when ( null ( nth ( - move_position 9 ) state ) )

					

					)

				)

			)

		)

	)

)
