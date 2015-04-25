;Global Variables
(defvar *missionaries*)	;Number of missionaries
(defvar *cannibals*)	;Number of cannibals

;main program
(defun main()
;if args not equal to 2 return
	(cond
		((= (length *args*) 2) 
		;initializes globals
			(setf *missionaries* (parse-integer (first *args*)) 
			*cannibals* (parse-integer (second *args*)))
			(m-c *missionaries* *cannibals*)
		)
		;or print usage
		(t
			(format t "~%Missionary vs Cannibal problem~%")
			(format t "---------- -- -------- -------~%~%")
			(format t "Usage: (m-c missionaries cannibals)~%")
			(format t "	missionaries - number of missionaries trying to cross~%")
			(format t "	cannibals    - number of cannibals trying to cross~%~%")
			;(error "Incorrect number of command line arguments~%~%")
		)
	)
)

;Missionaries and Cannibals problem
(defun m-c (missionaries cannibals)
	;check for unsolvable problem instance
	(when (and (< missionaries cannibals) (not (= missionaries 0)))
		(return-from m-c "Too few missionaries!"))

	;initialize global variables
	(setq *missionaries* missionaries)
	(setq *cannibals* cannibals)
	;Have we reached the goal? (all missionaries and cannibals on the right bank)
	(setf goal-state '(0 0 1))

	;Solve missionaries and cannibals problem using DFS
        (setf start-state '(0 0 0))
        ;The thrid argument for start state is which side of the bank the canoe is on
	;0 for left 1 for right
	(setf (first start-state) missionaries (second start-state) cannibals 
		(third start-state) 0)
	(format t "~%After start-state call.~%")
        (format t "Missionaries ~S~%" missionaries)
        (format t "Cannibals ~S~%" cannibals)
        (format t "Start-state ~S~%" start-state)
	(setf correct-path (list start-state))
	(cond 
		((generate-left-bank start-state)
			(format t "Success!~%")
			(format t "Path: ~S~%" correct-path))
		(t
			(format t "Failure!~%")
			(format t "No possible solutions~%")
		)
	)

	;suppress printing NIL upon return to interpreter
	(values)
)

;generate-left-bank returns a list of successor to the current state.
(defun generate-left-bank (state)
	;define local variables
	(let (m c b (miss (first state)) (cann (second state)) (boat (third state)) 
		(left-bank nil))

		(setf done nil)

		;(format t "Boat: ~S~%" boat)
		;(format t "left-bank: ~S~%" left-bank)
		;(format t "correct-path ~S~%" correct-path)

		(format t "before move 2 missionaries")

		;move 2 missionaries
		(cond ((and (= boat 0)(>= miss 2))
			(setq left-bank  (cons (list (- miss 2) cann 1) left-bank))
			(setq right-bank 
				(cons 
					(list (+ 2 (first (first right-bank))) 
					(- *cannibals* cann)
				) right-bank)
			)
		(format t "Moving 2 missionaries: ~S~%" left-bank)
		(format t "Right-bank: ~S~%" right-bank)
		))

		(format t "asdfghj move 2 missionaries")

		;move 2 cannibals
		(cond ((and (= boat 0) (>= cann 2))
			(setq left-bank  (cons (list miss (- cann 2) 1) left-bank))
			(setq right-bank 
				(cons 
					(list (- *missionaries* miss) 
					(+ 2 (second (first right-bank)))
				) right-bank)
			)
		(format t "Moving 2 cannibals: ~S~%" left-bank)
		(format t "Right-bank: ~S~%" right-bank)
		))

		;move 1 of each
		(cond ((and (= boat 0) (>= miss 1)(>= cann 1))
			(setq left-bank 
				(cons (list (decf miss) (decf cann) 1) left-bank))
			(setq right-bank 
				(cons 
					(list (+ 1 (first (first right-bank))) 
					(+ 1 (second (first right-bank)))
				) right-bank)
			)
		(format t "Moving 1 of each: ~S~%" left-bank)
		(format t "Right-bank: ~S~%" right-bank)
		))

		;move 1 missionary
		(cond ((and (= boat 0)(>= miss 1))
			(setq left-bank (cons (list (- miss 1) cann 1) left-bank))
			(setq right-bank 
				(cons 
					(list (+ 1 (first (first right-bank))) 
					(- *cannibals* cann)
				) right-bank)
			)
		(format t "Moving 1 missionary: ~S~%" left-bank)
		(format t "Right-bank: ~S~%" right-bank)
		))

		;move 1 cannibal
		(cond ((and (= boat 0) (>= cann 1))
			(setq left-bank (cons (list miss (- cann 1) 1) left-bank))
			(setq right-bank 
				(cons 
					(list (- *missionaries* miss) 
					(+ 1 (second (first right-bank)))
				) right-bank)
			)
		(format t "Moving 1 cannibal: ~S~%" left-bank)
		(format t "Right-bank: ~S~%" right-bank)
		))

		;move 1 missionary back
		(cond ((and (= boat 1) (> *missionaries* (first state)))
			(setq left-bank (cons (list (incf miss) cann 0) left-bank))
			(setq right-bank 
				(cons 
					(list (+ 1 (first (first right-bank))) 
					(- *cannibals* cann)
				) right-bank)
			)
		(format t "Moving 1 missionary back: ~S~%" left-bank)
		(format t "Right-bank: ~S~%" right-bank)
		))

		;move 1 cannibal back
		(cond (and (= boat 1) (> *cannibals* (second state)))
			(setq left-bank (cons (list miss (incf cann) 0) left-bank))
			(setq right-bank 
				(cons 
					(list (- *missionaries* miss) 
					(- 1 (second (first right-bank)))
				) right-bank)
			)
		(format t "Moving 1 cannibals back: ~S~%" left-bank)
		(format t "Right-bank: ~S~%" right-bank)
		))

		;move 1 of each back
		(cond ((and (= boat 1) 
			(> *missionaries* (first state)) 
			(> *cannibals* (second state)))
				(setq left-bank 
					(cons (list (decf miss) (decf cann) 1) left-bank))
				(setq right-bank 
					(cons 
						(list (- 1 (first (first right-bank))) 
						(- 1 (second (first right-bank)))
					) right-bank)
				)
		(format t "Moving 1 of each back: ~S~%" left-bank)
		(format t "Right-bank: ~S~%" right-bank)
		))

		;return list of left-bank, without duplicates
		(remove-duplicates left-bank :test #'equal)

		(nreverse left-bank)
		;(format t "Reversed left-bank: ~S~%" left-bank)
		;(format t "First left-bank ~S~%" (first left-bank))
		;(format t "What is in correct path ~S~%" correct-path)
		(loop while (and 
				(not (equal goal-state (first left-bank)))
				(not done)
			    )
					 do
			(setf miss (first (first left-bank)))
			(setf cann (second (first left-bank)))
			;(format t "Miss: ~S~%" miss)
			;(format t "Cann: ~S~%" cann)
			(cond 
				(or (and (< miss cann) (not (= miss 0))) 
					(and (< (first (first right-bank))
						(second (first right-bank)))
						(not (= (first (first right-bank)) 0))
					(pop left-bank))
				)
				(t (nconc correct-path (list (first left-bank)))
					(setq done 
						(generate-left-bank (first left-bank)))
				)
			)
			(when (eq left-bank NIL) (return-from generate-left-bank NIL))
		)
		(return-from generate-left-bank 1)
	)	
)

;Run missionaries and cannibals automatically upon loading file
(main)
