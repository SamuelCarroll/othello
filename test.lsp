;test multi flip left
(reset_brd)
(setf (nth 0 *GAME_BOARD*) 'W)
(setf (nth 1 *GAME_BOARD*) 'B)
(setf (nth 2 *GAME_BOARD*) 'B)
(setf (nth 3 *GAME_BOARD*) 'B)
(setf (nth 4 *GAME_BOARD*) 'B)
(setf (nth 5 *GAME_BOARD*) 'B)
(setf (nth 6 *GAME_BOARD*) 'B)
(prt_brd *GAME_BOARD*)
(format t "Please enter 1 8~%")
(move 'W)

(reset_brd)

;test multi flip right
(setf (nth 1 *GAME_BOARD*) 'B)
(setf (nth 2 *GAME_BOARD*) 'B)
(setf (nth 3 *GAME_BOARD*) 'B)
(setf (nth 4 *GAME_BOARD*) 'B)
(setf (nth 5 *GAME_BOARD*) 'B)
(setf (nth 6 *GAME_BOARD*) 'B)
(setf (nth 7 *GAME_BOARD*) 'W)
(prt_brd *GAME_BOARD*)
(format t "Please enter 1 1~%")
(move 'W)

(reset_brd)

;set first row
(setf (nth 0 *GAME_BOARD*) 'B)
(setf (nth 1 *GAME_BOARD*) 'B)
(setf (nth 2 *GAME_BOARD*) 'B)
(setf (nth 3 *GAME_BOARD*) 'B)
(setf (nth 4 *GAME_BOARD*) 'B)
(setf (nth 5 *GAME_BOARD*) 'B)
(setf (nth 6 *GAME_BOARD*) 'B)

; set second row
(setf (nth 8 *GAME_BOARD*) 'B)
(setf (nth 9 *GAME_BOARD*) 'W)
(setf (nth 10 *GAME_BOARD*) 'W)
(setf (nth 11 *GAME_BOARD*) 'W)
(setf (nth 12 *GAME_BOARD*) 'W)
(setf (nth 13 *GAME_BOARD*) 'W)
(setf (nth 14 *GAME_BOARD*) 'B)

;set third row
(setf (nth 16 *GAME_BOARD*) 'B)
(setf (nth 17 *GAME_BOARD*) 'W)
(setf (nth 18 *GAME_BOARD*) 'B)
(setf (nth 19 *GAME_BOARD*) 'B)
(setf (nth 20 *GAME_BOARD*) 'B)
(setf (nth 21 *GAME_BOARD*) 'W)
(setf (nth 22 *GAME_BOARD*) 'B)

;set fourth row
(setf (nth 24 *GAME_BOARD*) 'B)
(setf (nth 25 *GAME_BOARD*) 'W)
(setf (nth 26 *GAME_BOARD*) 'B)
(setf (nth 27 *GAME_BOARD*) '-)
(setf (nth 28 *GAME_BOARD*) 'B)
(setf (nth 29 *GAME_BOARD*) 'W)
(setf (nth 30 *GAME_BOARD*) 'B)

;set fifth row
(setf (nth 32 *GAME_BOARD*) 'B)
(setf (nth 33 *GAME_BOARD*) 'W)
(setf (nth 34 *GAME_BOARD*) 'B)
(setf (nth 35 *GAME_BOARD*) 'B)
(setf (nth 36 *GAME_BOARD*) 'B)
(setf (nth 37 *GAME_BOARD*) 'W)
(setf (nth 38 *GAME_BOARD*) 'B)

;set sixth row
(setf (nth 40 *GAME_BOARD*) 'B)
(setf (nth 41 *GAME_BOARD*) 'W)
(setf (nth 42 *GAME_BOARD*) 'W)
(setf (nth 43 *GAME_BOARD*) 'W)
(setf (nth 44 *GAME_BOARD*) 'W)
(setf (nth 45 *GAME_BOARD*) 'W)
(setf (nth 46 *GAME_BOARD*) 'B)

;set seventh row
(setf (nth 48 *GAME_BOARD*) 'B)
(setf (nth 49 *GAME_BOARD*) 'B)
(setf (nth 50 *GAME_BOARD*) 'B)
(setf (nth 51 *GAME_BOARD*) 'B)
(setf (nth 52 *GAME_BOARD*) 'B)
(setf (nth 53 *GAME_BOARD*) 'B)
(setf (nth 54 *GAME_BOARD*) 'B)

;make move
(prt_brd *GAME_BOARD*)
(format t "Please enter 4 4~%")
(move 'W)
