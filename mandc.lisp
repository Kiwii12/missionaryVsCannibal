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
	(setf right-bank '(0 0))

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
		((generate-successors start-state)
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

;generate-successors returns a list of successor to the current state.
(defun generate-successors (state)
	;define local variables
	(let (m1 c1 b1 (miss (first state)) (cann (second state)) (boat (third state)) 
		(successors nil))

		(setf done nil)

		;(format t "Boat: ~S~%" boat)
		;(format t "successors: ~S~%" successors)
		;(format t "correct-path ~S~%" correct-path)

		;move 2 missionaries
		(when (and (= boat 0)(>= miss 2))
			(setq successors (cons (list (- miss 2) cann 1) successors))
			(setf (nth 0 right-bank) (+ (first right-bank) 2))
		(format t "Moving 2 missionaries: ~S~%" successors))
		
		;move 2 cannibals
		(when (and (= boat 0) (>= cann 2))
			(setq successors (cons (list miss (- cann 2) 1) successors))
			(setf (nth 1 right-bank) (+ (second right-bank) 2))
		(format t "Moving 2 cannibals: ~S~%" successors))

		;move 1 of each
		(when (and (= boat 0) (>= miss 1)(>= cann 1))
			(setq successors 
				(cons (list (decf miss) (decf cann) 1) successors))
			(setf (nth 0 right-bank) (+ (first right-bank) 1))
			(setf (nth 1 right-bank) (+ (first right-bank) 1))
		(format t "Moving 1 of each: ~S~%" successors))

		;move 1 missionary
		(when (and (= boat 0)(>= miss 1))
			(setq successors (cons (list (- miss 1) cann 1) successors))
			(setf (nth 0 right-bank) (+ (first right-bank) 1))
		(format t "Moving 1 missionary: ~S~%" successors))

		;move 1 cannibal
		(when (and (= boat 0) (>= cann 1))
			(setq successors (cons (list miss (- cann 1) 1) successors))
			(setf (nth 1 right-bank) (+ (first right-bank) 1))
		(format t "Moving 1 cannibal: ~S~%" successors))

		;move 1 missionary back
		(when (and (= boat 1) (> *missionaries* (first state)))
			(setq successors (cons (list (incf miss) cann 0) successors))
			(setf (nth 0 right-bank) (- (first right-bank) 1))
		(format t "Moving 1 missionary back: ~S~%" successors))

		;move 1 cannibal back
		(when (and (= boat 1) (> *cannibals* (second state)))
			(setq successors (cons (list miss (incf cann) 0) successors))
			(setf (nth 1 right-bank) (+ (first right-bank) 1))
		(format t "Moving 1 cannibals back: ~S~%" successors))

		;move 1 of each back
		(when (and (= boat 1) 
			(> *missionaries* (first state)) 
			(> *cannibals* (second state)))
				(setq successors 
					(cons (list (decf miss) (decf cann) 1) successors))
			(setf (nth 0 right-bank) (- (first right-bank) 1))
			(setf (nth 1 right-bank) (- (first right-bank) 1))
		(format t "Moving 1 of each back: ~S~%" successors))

		;return list of successors, without duplicates
		(remove-duplicates successors :test #'equal)

		(nreverse successors)
		;(format t "Reversed successors: ~S~%" successors)
		;(format t "First successors ~S~%" (first successors))
		;(format t "What is in correct path ~S~%" correct-path)
		(loop while (and 
				(not (= goal-state (first successors)))
				(not done)
				(
			    )
					 do
			(setf miss (first (first successors)))
			(setf cann (second (first successors)))
			;(format t "Miss: ~S~%" miss)
			;(format t "Cann: ~S~%" cann)
			(cond 
				((or (> miss cann) (= miss 0))
					(nconc correct-path (list (first successors)))
					(setq done 
						(generate-successors (first successors)))
				)
				(t (pop successors))
			)
			(when (eq successors NIL) (return-from generate-successors NIL))
		)
		(return-from generate-successors 1)
	)	
)

;Run missionaries and cannibals automatically upon loading file
(main)
