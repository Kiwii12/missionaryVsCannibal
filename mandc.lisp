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
	(when (and (< missionaries cannibals) (not (equalp missionaries 0))
		(return-from m-c "Too few missionaries!"))

	;initialize global variables
	(setq *missionaries* missionaries)
	(setq *cannibals* cannibals)

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
	(generate-successors start-state)
	(format t "Success!~%")
	(format t "Path: ~S~%" correct-path)

	;suppress printing NIL upon return to interpreter
	(values)
)

;Have we reached the goal? (all missionaries and cannibals on the right bank)
(setf goal-state '(0 0 1))

(setf correct-path ())

;generate-successors returns a list of successor to the current state.
(defun generate-successors (state)
	;define local variables
	(let (m1 c1 b1 (miss (first state)) (cann (second state)) (boat (third state)) 
		(succs nil))

		(setf done nil)

		(format t "Boat: ~S~%" boat)
		(format t "Succs: ~S~%" succs)

#|		(cond
			((and (equalp miss 2) (equalp cann 0) (equalp boat 0))
				(setq correct-path (first succs))
				(return-from generate-successors)
			)
			((and (equalp miss 0) (equalp cann 2) (equalp boat 0))
				(setq correct-path (first succs))
				(return-from generate-successors)
			)
			((and (equalp miss 1) (equalp cann 1) (equalp boat 0))
				(setq correct-path (first succs))
				(return-from generate-successors)
			)
			((and (equalp miss 1) (equalp cann 0) (equalp boat 0))
				(setq correct-path (first succs))
				(return-from generate-successors)
			)
			((and (equalp miss 0) (equalp cann 1) (equalp boat 0))
				(setq correct-path (first succs))
				(return-from generate-successors)
			)
		)|#

		#|;move 2 missionaries
		(when (and (eql boat 0) (>= miss 2))
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

		;move 1 missionary
		;move 1 cannibal

		;move 1 missionary back
		(when (and (eql boat 1) (> *missionaries* (first state)))
			(setq succs (cons (list (incf miss) cann 0) succs))
		(format t "Moving 1 missionary back: ~S~%" succs))

		;move 1 cannibal back
		(when (and (eql boat 1) (> *cannibals* (second state)))
			(setq succs (cons (list miss (incf cann) 0) succs))
		(format t "Moving 1 cannibals back: ~S~%" succs))

		;return list of successors, without duplicates
		(remove-duplicates succs :test #'equal)
|#
		(nreverse succs)
		(format t "Reversed succs: ~S~%" succs)
		(format t "First succs ~S~%" (first succs))
		(format t "What is in correct path ~S~%" correct-path)
		#|(loop while (or 
				(not (equalp goal-state (first succs)))
				(not done)
			    )
					 do
			(setf miss (first (first succs)))
			(setf cann (second (first succs)))
			(format t "Miss: ~S~%" miss)
			(format t "Cann: ~S~%" cann)
			(cond 
				((or (> miss cann) (equalp miss 0))
					(setq correct-path (first succs))
					(setq done (generate-successors (first succs)))
				)
				(t (pop succs))
			)
		)|#
		(setq correct-path (first succs))
		(return-from generate-successors)
		(setf done t)
	)	
)

;Run missionaries and cannibals automatically upon loading file
(main)
