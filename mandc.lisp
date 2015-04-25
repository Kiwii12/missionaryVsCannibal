;Global Variables
(defvar *missionaries*)	;Number of missionaries
(defvar *cannibals*)	;Number of cannibals

;main program
(defun main()
;if args not equal to 2 return
	(cond
		((= (length *args*) 2) 
		;initializes globals
			(setf *missionaries* (parse-integer (car *args*)) 
			*cannibals* (parse-integer (cdr *args*)))
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
	(when (< missionaries cannibals)(return-from m-c "Too few missionaries!"))

	;initialize global variables
	(setq *missionaries* missionaries)
	(setq *cannibals* cannibals)

	;Solve missionaries and cannibals problem using DFS
        (setf start-state '(0 0 0))
        ;The thrid argument for start state is which side of the bank the canoe is on
	;0 for left 1 for right
	(setf (car start-state) missionaries (cadr start-state) cannibals 
		(caddr start-state) 0)
	(format t "~%After start-state call.~%")
        (format t "Missionaries ~S~%" missionaries)
        (format t "Cannibals ~S~%" cannibals)
        (format t "Start-state ~S~%" start-state)
	(generate-successors start-state)

	;suppress printing NIL upon return to interpreter
	(values)
)

;Have we reached the goal? (all missionaries and cannibals on the right bank)
(setf goal-state '(0 0 1))

;generate-successors returns a list of successor to the current state.
(defun generate-successors (state)
	;define local variables
	(let (m1 c1 b1 (miss (car state)) (cann (cadr state)) (boat (caddr state)) 
		(succs nil))

		(format t "Boat: ~S~%" boat)
		(format t "Succs: ~S~%" succs)

		;move 2 missionaries
		(when (and (eql (caddr state) 0) (>= miss 2))
			(setq succs (cons (list (- miss 2) cann 1) succs))
		(format t "Moving 2 missionaries: ~S~%" succs))
		
		;move 2 cannibals
		(when (and (eql boat 0) (>= cann 2))
			(setq succs (cons (list miss (- cann 2) 1) succs))
		(format t "Moving 2 cannibals: ~S~%" succs))

		;move 1 of each
		(when (and (eql boat 0) (>= miss 1)(>= cann 1))
			(setq succs (cons (list (decf miss) (decf cann) 1) succs))
		(format t "Moving 1 of each: ~S~%" succs))

		;move 1 missionary back
		(when (eql boat 1) 
			(setq succs (cons (list (decf miss) cann 0) succs))
		(format t "Moving 1 missionary back: ~S~%" succs))

		;move 1 cannibal back
		(when (eql boat 1) 
			(setq succs (cons (list miss (decf cann) 0) succs))
		(format t "Moving 1 cannibals back: ~S~%" succs))

		;return list of successors, without duplicates
		(remove-duplicates succs :test #'equal)
		(nreverse succs)
		(format t "Reversed succs: ~S~%" succs)
		(loop while (not (equalp goal-state (car succs))) do
			(setf miss (car (car succs)))
			(setf cann (car (cdr (car succs))))
			(format t "Miss: ~S~%" miss)
			(format t "Cann: ~S~%" cann)
			(if (< miss cann) 
				(pop succs)
				(generate-successors (car succs)))
		)
	)	
)
;Run WaterJug automatically upon loading file
(main)
