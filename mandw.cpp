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
	(format t "This is where DFS code goes.")
	(start-state)
	(format t "After start-state call.")
	(generate-successors (start-state))

	;suppress printing NIL upon return to interpreter
	(values)
)

;Define the start state
(defun start-state () '(missionaries cannibals left))

;Have we reached the goal? (all missionaries and cannibals on the right bank)
(defun goal-state () '(0 0 right))

;generate-successors returns a list of successor to the current state.
(defun generate-successors (state)
	;define local variables
	(let (m1 c1 b1 (miss (car state)) (cann (cadr state)) (boat (caddr state)) 
		(succs nil))

		;move 2 missionaries
		(when (string-equal boat "left") (setq succs 
			(cons (list (- miss 2) cann right) succs)))
		
		;move 2 cannibals
		(when (string-equal boat "left") (setq succs 
			(cons (list miss (- cann 2) right) succs)))

		;move 1 of each
		(when (string-equal boat "left") (setq succs 
			(cons (list (decf miss) (decf cann) right) succs)))

		;move 1 missionary back
		(when (string-equal boat "right") (setq succs 
			(cons (list (decf miss) cann left) succs)))

		;move 1 cannibal back
		(when (string-equal boat "right") (setq succs 
			(cons (list miss (decf cann) left) succs)))

		;return list of successors, without duplicates
		(remove-duplicates succs :test #'equal)
	)
)
;Run WaterJug automatically upon loading file
(main)
