; Constants required to define difference between player and computer
(defconstant *player* 'X)
(defconstant *comp* 'O)
(defconstant *empty* '_)

; Used to initialize the board
(defun init-list ()
    (make-array '(16)
       :initial-contents '(_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _))
) 

; Compares passed value to an empty space, returns NIL if not empty
(defun empty-place (move board)
  (equal *empty* (row-major-aref board move))
)

; Returns list of legal placements, or empty spots for passed in board
(defun legal-moves (board)
  (loop for move below (array-total-size board)
     when (empty-place move board) collect move) ; Calls empty-place function to compare 
)

; Used to generate scores/terminal states for minmax algorithm 
(defun state-check (board)
    (cond 
        ((is-game-over-player board) 1) ; if player wins, +1
        ((is-game-over-comp board) -1)  ; if computer wins, -1 (what we want for MINMAX)
        (t 0))                          ; otheriwise, nothing.
)

; Returns a utility state
(defun max-value (board moves-list) ; board holds copy of original game board, moves list holds legal moves
    (print "MAX-VALUE")
    (print-board board)
        
    (setq move (car moves-list))    ; for second condition
    
    (print "MAX-VALUE LEGAL MOVES")
    (print moves-list)
    
    (loop for moves below (list-length moves-list) ; for-loop for finding max actions
        do (if (not (or 
                    (equal (state-check board) 1)
                    (equal (state-check board) -1)  ; if not in terminal state
                    (equal (legal-moves board) NIL) ; and there is still more legal moves
                )
            )
            (progn 
                (setf (aref board (car moves-list)) *player*)   ; place a marker at the first legal move spot
                (min-value board (cdr moves-list))              ; move onto next index
            )            
        )
    )

    (print "MAX-VALUE")    
    (print-board board)
    (write (state-check board))         ; return terminal state if not in terminal state
)

; Returns a utility state
(defun min-value (board moves-list) ; board holds copy of original game board, moves list holds legal moves
    (print "MIN-VALUE")
    (print-board board)

    (setq move (car moves-list))    ; for second condition
    
    (print "MIN-VALUE LEGAL MOVES")
    (print moves-list)

    (loop for moves below (list-length moves-list) ; for-loop for finding min actions
        do (if (not (or 
                    (equal (state-check board) 1)
                    (equal (state-check board) -1)      ; if not in terminal state
                    (equal (legal-moves board) NIL)     ; and there is still more legal moves
                )
            )
            (progn 
                (setf (aref board (car moves-list)) *comp*)     ; place a marker at the first legal move spot 
                (max-value board (cdr moves-list))              ; move onto next index
            )            
        )
    )


    (print "MIN-VALUE")    
    (print-board board)
    (write (state-check board))         ; return terminal state if not in terminal state
)

; Returns action that Min (or computer) shoud make.
(defun minmax (board move-list)        ; Board is board, move-list is a list of legal moves based off of  board


    (setq test-board (copy-seq board)) ; Make a seperate board to find terminal/goal states 

    (setq score-list ())               ; To hold list of scores, or actions for each index passed into min-value
                                       ; Each index is a score that corresponds to each index in legal move
                                       ; i.e  a score of -1 in index 0 of score-list corresponds to making a move in move-list at index 0

    (setq move 0)
    (loop for moves below (list-length move-list)
        do (if (not (or 
                    (equal (legal-moves test-board) NIL) ; and there is still more legal moves
                )
            )
            (progn 

                (setq score (min-value test-board move-list))      ; algorithm starts with min-value 
                
                (setq move (car move-list))                        ; used to find index that makes winning move

                (push score score-list)                            ; add the score to score list

                (print-board test-board)

                (setq move-list (cdr move-list))                   ; pop off first index to evaluate next index
      

                (if (equal (state-check test-board) 1)
                    (setq test-board (copy-seq board)) ; reset board
                )
                (if (equal (state-check test-board) -1)
                    (return )                                    ; if we find an a terminal state in favor of MIN, stop!
                ) 
        )   )
    )

    (setf (aref board move) *comp*)   ; place a marker at the first legal move spot
)

; Checks if comp won
(defun is-game-over-comp (board)
        ; HORIZONTAL COMIBINATIONS
    (or (is-combo-comp (aref board 0) (aref board 1) (aref board 2)) 
        (is-combo-comp (aref board 1) (aref board 2) (aref board 3))        
        (is-combo-comp (aref board 4) (aref board 5) (aref board 6))   
        (is-combo-comp (aref board 5) (aref board 6) (aref board 7)) 
        (is-combo-comp (aref board 8) (aref board 9) (aref board 10))
        (is-combo-comp (aref board 9) (aref board 10) (aref board 11))
        (is-combo-comp (aref board 12) (aref board 13) (aref board 14))   
        (is-combo-comp (aref board 13) (aref board 14) (aref board 15))
        ; VERTICAL COMBINATIONS
        (is-combo-comp (aref board 0) (aref board 4) (aref board 8))
        (is-combo-comp (aref board 4) (aref board 8) (aref board 12))
        (is-combo-comp (aref board 1) (aref board 5) (aref board 9))
        (is-combo-comp (aref board 5) (aref board 9) (aref board 13))
        (is-combo-comp (aref board 2) (aref board 6) (aref board 10))
        (is-combo-comp (aref board 6) (aref board 10) (aref board 14))
        (is-combo-comp (aref board 3) (aref board 7) (aref board 11))
        (is-combo-comp (aref board 7) (aref board 11) (aref board 15))
        ; DIAGONAL COMBINATIONS
        (is-combo-comp (aref board 0) (aref board 5) (aref board 10))
        (is-combo-comp (aref board 5) (aref board 10) (aref board 15))
        (is-combo-comp (aref board 4) (aref board 9) (aref board 14))
        (is-combo-comp (aref board 1) (aref board 6) (aref board 11))
        (is-combo-comp (aref board 2) (aref board 5) (aref board 8))
        (is-combo-comp (aref board 3) (aref board 6) (aref board 9))
        (is-combo-comp (aref board 6) (aref board 9) (aref board 12))
        (is-combo-comp (aref board 7) (aref board 10) (aref board 13)) 
    )
)

; Checks if player won
(defun is-game-over-player (board)
        ; HORIZONTAL COMIBINATIONS
    (or (is-combo-player (aref board 0) (aref board 1) (aref board 2)) 
        (is-combo-player (aref board 1) (aref board 2) (aref board 3))        
        (is-combo-player (aref board 4) (aref board 5) (aref board 6))   
        (is-combo-player (aref board 5) (aref board 6) (aref board 7)) 
        (is-combo-player (aref board 8) (aref board 9) (aref board 10))
        (is-combo-player (aref board 9) (aref board 10) (aref board 11))
        (is-combo-player (aref board 12) (aref board 13) (aref board 14))   
        (is-combo-player (aref board 13) (aref board 14) (aref board 15))
        ; VERTICAL COMBINATIONS
        (is-combo-player (aref board 0) (aref board 4) (aref board 8))
        (is-combo-player (aref board 4) (aref board 8) (aref board 12))
        (is-combo-player (aref board 1) (aref board 5) (aref board 9))
        (is-combo-player (aref board 5) (aref board 9) (aref board 13))
        (is-combo-player (aref board 2) (aref board 6) (aref board 10))
        (is-combo-player (aref board 6) (aref board 10) (aref board 14))
        (is-combo-player (aref board 3) (aref board 7) (aref board 11))
        (is-combo-player (aref board 7) (aref board 11) (aref board 15))
        ; DIAGONAL COMBINATIONS
        (is-combo-player (aref board 0) (aref board 5) (aref board 10))
        (is-combo-player (aref board 5) (aref board 10) (aref board 15))
        (is-combo-player (aref board 4) (aref board 9) (aref board 14))
        (is-combo-player (aref board 1) (aref board 6) (aref board 11))
        (is-combo-player (aref board 2) (aref board 5) (aref board 8))
        (is-combo-player (aref board 3) (aref board 6) (aref board 9))
        (is-combo-player (aref board 6) (aref board 9) (aref board 12))
        (is-combo-player (aref board 7) (aref board 10) (aref board 13)) 
    )
)

; Checks combinations for computer, ignores _ spaces.
(defun is-combo-comp (a b c)
    (and (equal a b)
         (equal a c)
         (equal a *comp*)
         (equal b *comp*)
         (equal c *comp*))
)

; Checks combinations for player, ignores _ spaces.
(defun is-combo-player (a b c)
    (and (equal a b)
         (equal a c)
         (equal a *player*)
         (equal b *player*)
         (equal c *player*))
)

; Checks if move is within boundaries & is not taken 
(defun check-if-valid-move (board input)
    (and (<= input '16) (>= input '0) (equal (aref board input) *empty*))
) 

; Prints out the Board
(defun print-board (board)
    (format t " ~%")
    (format t "~a ~a ~a ~a ~%" (aref board 0) (aref board 1) (aref board 2) (aref board 3))
    (format t "~a ~a ~a ~a ~%" (aref board 4) (aref board 5) (aref board 6) (aref board 7))
    (format t "~a ~a ~a ~a ~%" (aref board 8) (aref board 9) (aref board 10) (aref board 11))
    (format t "~a ~a ~a ~a ~%" (aref board 12) (aref board 13) (aref board 14) (aref board 15))
    (format t " ~%")
)

; Reads and define's the players move
(defun player-move (board)
    (format t "Enter in # where you want to place X (0-15): ")    
    (setq move (read))
    (if (check-if-valid-move board move)
        (setf (aref board move) *player*)
    (format t "ERROR! Please enter in an integer that is <= 0, >= 16, AND is in an empty space. ERROR!~%"))
)

; The game
(defun ttt ()
    (setq board (init-list))
    (format t "-------- GAME BOARD --------~% ")    
    (print-board board)
    (format t "-------- GAME BOARD --------~% ")    

    (loop 
        (if (not (or (is-game-over-player board)
                     (is-game-over-comp board) 
                     (equal (legal-moves board) NIL)
                 )
            )
            (progn               
                (player-move board)                 
                (minmax board (legal-moves board))
                (format t "-------- GAME BOARD --------~% ")    
                (print-board board)
                (format t "-------- GAME BOARD --------~% ")    
            )
        )
        (when 
            (or (is-game-over-player board)
                (is-game-over-comp board) 
                (equal (legal-moves board) NIL))
            (return "~%")
        )
    )
    (print "game over. :) if you won. :( if you lost")
)

(ttt)
