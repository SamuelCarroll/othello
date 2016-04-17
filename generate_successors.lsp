(defun generate_successors (state color)

	( let ( ( succ '() ) move_position match_position ( row_position 0 ) )

		( dolist ( row state )
			
			( dolist ( col row )

				( when ( and ( not ( equal col color ) ) ( not ( null col ) ) )

					( setf match_position ( position col state ) )

					( when ( equal ( nth ( - move_position 1 ) row ) color )

						( dotimes ( repeats ( - 8 move_position ) t )
							( when ( nth ( + match_position repeats ) row )
								( cons move_position ( + match_position repeats ) )
								( cons move_position row_position )
							)
						)

					)

					( when ( equal ( nth ( + move_position 1 ) row ) color )

						( dotimes ( repeats ( - 8 move_position ) t )
							( when ( nth ( - match_position repeats ) row )
								( cons move_position ( - match_position repeats ) )
								( cons move_position row_position )
							)
						)

					)

					( when ( equal ( nth move_position ( nth ( + row_position 1 ) state ) ) color )

						( dotimes ( repeats ( - 8 row_position ) t )
							( when ( nth move_position ( nth ( - row_position repeats ) state ) )
								( cons move_position match_position )
								( cons move_position ( - row_position repeats ) )
							)
						)

					)

					( when ( equal ( nth move_position  ( nth ( - row_position 1 ) state ) ) color )

						( cond

							( when 
								( dotimes ( repeats ( - 8 row_position ) t )
									( when ( nth move_position ( nth ( + row_position repeats ) state ) )
										( cons move_position match_position )
										( cons move_position ( + row_position repeats ) )
									)
								)
							)

							(
								( dotimes ( repeats ( - 8 row_position ) t )
									( when ( nth move_position ( nth ( + row_position repeats ) state ) )
										( cons move_position match_position )
										( cons move_position ( + row_position repeats ) )
									)
								)
							)
						)

					)

					( when ( equal ( nth ( - move_position 1 )  ( nth ( + row_position 1 ) state ) ) color )

						( cond

							( when 
								( dotimes ( repeats ( - 8 row_position ) t )
									( when ( nth move_position ( nth ( + row_position repeats ) state ) )
										( cons move_position match_position )
										( cons move_position ( + row_position repeats ) )
									)
								)
							)

							(
								( dotimes ( repeats ( - 8 row_position ) t )
									( when ( nth move_position ( nth ( + row_position repeats ) state ) )
										( cons move_position match_position )
										( cons move_position ( + row_position repeats ) )
									)
								)
							)
						)

					)

					( when ( equal ( nth ( + move_position 1 )  ( nth ( + row_position 1 ) state ) ) color )

						( cond

							( when 
								( dotimes ( repeats ( - 8 row_position ) t )
									( when ( nth move_position ( nth ( + row_position repeats ) state ) )
										( cons move_position match_position )
										( cons move_position ( + row_position repeats ) )
									)
								)
							)

							(
								( dotimes ( repeats ( - 8 row_position ) t )
									( when ( nth move_position ( nth ( + row_position repeats ) state ) )
										( cons move_position match_position )
										( cons move_position ( + row_position repeats ) )
									)
								)
							)
						)

					)

					( when ( equal ( nth ( + move_position 1 )  ( nth ( - row_position 1 ) state ) ) color )

						( cond

							( when 
								( dotimes ( repeats ( - 8 row_position ) t )
									( when ( nth move_position ( nth ( + row_position repeats ) state ) )
										( cons move_position match_position )
										( cons move_position ( + row_position repeats ) )
									)
								)
							)

							(
								( dotimes ( repeats ( - 8 row_position ) t )
									( when ( nth move_position ( nth ( + row_position repeats ) state ) )
										( cons move_position match_position )
										( cons move_position ( + row_position repeats ) )
									)
								)
							)
						)

					)

					( when ( equal ( nth ( - move_position 1 )  ( nth ( - row_position 1 ) state ) ) color )

						( cond

							( when 
								( dotimes ( repeats ( - 8 row_position ) t )
									( when ( nth move_position ( nth ( + row_position repeats ) state ) )
										( cons move_position match_position )
										( cons move_position ( + row_position repeats ) )
									)
								)
							)

							(
								( dotimes ( repeats ( - 8 row_position ) t )
									( when ( nth move_position ( nth ( + row_position repeats ) state ) )
										( cons move_position match_position )
										( cons move_position ( + row_position repeats ) )
									)
								)
							)
						)

					)

				)

				( setf row_position ( 1+ row_position ) )

			)

		)

	)

)
