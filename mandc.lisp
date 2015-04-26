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
	(setf r-bank '(0 0))

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
	(setf left-bank (list start-state))
	(setf right-bank (list (list 0 0)))
	(cond 
		((generate-l-bank start-state r-bank)
			(format t "Success!~%")
			(format t "Left-bank Path: ~S~%" left-bank)
			(format t "Right-bank Path: ~S~%" right-bank))
		(t
			(format t "Failure!~%")
			(format t "No possible solutions~%")
		)
	)

	;suppress printing NIL upon return to interpreter
	(values)
)

;generate-l-bank returns a list of successor to the current state.
(defun generate-l-bank (l-bank r-bank)
	;define local variables
	(let ((miss (first l-bank)) (cann (second l-bank)) 
		(boat (third l-bank)) (m (first r-bank)) (c (second r-bank)) 
		(l-bank nil) (r-bank nil))

		(setf done nil)

		;(format t "Boat: ~S~%" boat)
		(format t "l-bank: ~S~%" l-bank)
		(format t "r-bank: ~S~%" r-bank)
		(format t "left-bank ~S~%" left-bank)
		(format t "right-bank ~S~%" right-bank)

		;move 2 missionaries
		(cond ((and (= boat 0)(>= miss 2)(>= (- miss 2) cann))
			(setq l-bank  (cons (list (- miss 2) cann 1) l-bank))
			(setq r-bank (cons (list (+ 2 m) (- *cannibals* cann)) r-bank))
		(format t "Moving 2 missionaries: ~S~%" l-bank)
		(format t "r-bank: ~S~%" r-bank)
		))

		;move 2 cannibals
		(cond ((and (= boat 0) (>= cann 2))
			(setq l-bank (cons (list miss (- cann 2) 1) l-bank))
			(setq r-bank (cons (list (- *missionaries* miss) (+ 2 c)) r-bank))
		(format t "Moving 2 cannibals: ~S~%" l-bank)
		(format t "r-bank: ~S~%" r-bank)
		))

		;move 1 of each
		(cond ((and (= boat 0) (>= miss 1)(>= cann 1))
			(setq l-bank (cons (list (- miss 1) (- cann 1) 1) l-bank))
			(setq r-bank (cons (list (+ 1 m)(+ 1 c)) r-bank))
		(format t "Moving 1 of each: ~S~%" l-bank)
		(format t "r-bank: ~S~%" r-bank)
		))

		;move 1 missionary
		(cond ((and (= boat 0)(>= miss 1)(>= (- miss 1) cann))
			(setq l-bank (cons (list (- miss 1) cann 1) l-bank))
			(setq r-bank (cons (list (+ 1 m)c) r-bank))
		(format t "Moving 1 missionary: ~S~%" l-bank)
		(format t "r-bank: ~S~%" r-bank)
		))

		;move 1 cannibal
		(cond ((and (= boat 0) (>= cann 1))
			(setq l-bank (cons (list miss (- cann 1) 1) l-bank))
			(setq r-bank (cons (list m (+ 1 c)) r-bank))
		(format t "Moving 1 cannibal: ~S~%" l-bank)
		(format t "r-bank: ~S~%" r-bank)
		))

		;move 1 missionary back
		(cond ((and (= boat 1) (>= m 1))
			(setq l-bank (cons (list (+ miss 1) cann 0) l-bank))
			(setq r-bank (cons (list (- m 1) c) r-bank))
		(format t "Moving 1 missionary back: ~S~%" l-bank)
		(format t "r-bank: ~S~%" r-bank)
		))

		;move 1 cannibal back
		(cond ((and (= boat 1) (>= c 1))
			(setq l-bank (cons (list miss (+ cann 1) 0) l-bank))
			(setq r-bank (cons (list m (- c 1)) r-bank))
		(format t "Moving 1 cannibals back: ~S~%" l-bank)
		(format t "r-bank: ~S~%" r-bank)
		))

		;move 1 of each back
		(cond ((and (= boat 1) 
			(>= m 1)
			(>= c 1))
				(setq l-bank (cons (list (+ miss 1)(+ cann 1) 0) l-bank))
				(setq r-bank (cons (list (- m 1)(- c 1)) r-bank))
		(format t "Moving 1 of each back: ~S~%" l-bank)
		(format t "r-bank: ~S~%" r-bank)
		))

		;return list of l-bank, without duplicates
		(remove-duplicates l-bank :test #'equal)

		(nreverse l-bank)
		(nreverse r-bank)
		(format t "triple test~%")
		(setf temp-path (copy-list left-bank))
		(setf length-of-left-bank (length temp-path))
		(setf length-of-l-bank (length l-bank))
		;To catch the inifite loop
		(loop as i from 0 to length-of-left-bank do 
			(loop as j from 0 to length-of-l-bank do
				(cond 
					((equalp (first temp-path)(first l-bank))
						(pop l-bank)
						(nconc temp-path (list (first temp-path)))
						(pop temp-path)
						(setf length-of-l-bank (length l-bank))
					)
					(t
						(nconc temp-path (list (first temp-path)))
						(pop temp-path)
					)
				)
			)
			(nconc l-bank (list (first l-bank)))
			(pop l-bank)
		)
		(nconc l-bank (list (first l-bank)))
		(pop l-bank)
		(loop while (and (not (equal goal-state (first l-bank))) (not done)) do
			(setf miss (first (first l-bank)))
			(setf cann (second (first l-bank)))
			(setf m (first (first r-bank)))
			(setf c (second (first r-bank)))
			(format t "miss: ~S~%" miss)
			(format t "cann: ~S~%" cann)
			(format t "m: ~S~%" m)
			(format t "c: ~S~%" c)
			(cond
				((or (and (< miss cann) (not(= miss 0))) 
					(and (< m c) (not(= m 0))))
					(format t "test~%")
					(pop l-bank)
					(pop r-bank)
				)
				
				(t
					(format t "double test~%")
					(nconc left-bank (list (first l-bank)))
					(nconc right-bank (list (first r-bank)))
					(setq done 
						(generate-l-bank (first l-bank)
							(first r-bank)))
					(when done (return-from generate-l-bank 1))
				)
			)
			(when (eq l-bank NIL) (return-from generate-l-bank NIL))
		)
		(return-from generate-l-bank 1)
	)	
)

;Run missionaries and cannibals automatically upon loading file
(main)
