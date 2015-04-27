;Global Variables
(defvar *missionaries*)	;Number of missionaries
(defvar *cannibals*)	;Number of cannibals
(defvar *fail-count*)	;Count of attemps using the same case to trap loop

#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||##||
|	MAIN 
|
|	Authors-	Jason Anderson and Johnathan Ackerman
|
|	Description-	This function initiallizes the global variables used by
|				the program. It also outputs the usage statement and prints
|				out the table showing a solution for how the missionaries and
|				cannibals get across the river.
|
|	Param(in)-	*args* - list holds the initial values for *cannibals* and
|							*missionaries*
|##|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
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

#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||##||
|	M-C 
|
|	Authors-	Jason Anderson and Johnathan Ackerman
|
|	Description-	This function starts the recursive calls and determines if
|				the a potential path is possible.
|
|	Param(in)-	cannibals - the number of flesh eating heathens starting 
|								on the left bank hoping for a quick 
|								religious meal
|	Param(in)-	missionaries - the number of religious folk attempting 
|									to "save" flesh eating heathens
|##|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
;Missionaries and Cannibals problem
(defun m-c (missionaries cannibals)
	;check for unsolvable problem instance
	(when (and (< missionaries cannibals) (not (= missionaries 0)))
		(return-from m-c "Too few missionaries!"))

	;initialize global variables
	(setf *missionaries* missionaries)
	(setf *cannibals* cannibals)
	(setf *fail-count* 0)

	(cond 
		((left-bank *missionaries* *cannibals*)
			(format t "Success!~%"))
		(t
			(format t "Failure!~%")
			(format t "No possible solutions~%")
		)
	)

	;suppress printing NIL upon return to interpreter
	(values)
)

#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||##||
|	LEFT-BANK 
|
|	Authors-	Jason Anderson and Johnathan Ackerman
|
|	Description-	This function determins what cases are valid and calls
|				right-bank to send the boat across the river.
|
|	Param(in)-	cann - the number of flesh eating heathens starting on the 
|							left bank hoping for a quick religious meal
|	Param(in)-	miss - the number of religious folk attempting to "save" 
|							flesh eating heathens
|
|	returns t-  the right bank returned true meaning a solution has been found
|
|	returns nil- no possible solution
|##|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
(defun left-bank (miss cann)
	(setf m miss)
	(setf c cann)
	(setf r-miss (- *missionaries* miss))
	(setf r-cann (- *cannibals* cann))
	(setf done NIL)
	(format t "m: ~S~%c: ~S~%r-miss: ~S~%r-cann: ~S~%" m c r-miss r-cann)

	;move 2 missionaries
	(when	(and 	(>= (- m 2) 0) (<= (+ r-miss 2) *missionaries*) 
			(or (>= (- m 2) c) (= (- m 2) 0)) (>= (+ r-miss 2) r-cann)
		)
		(format t "To right bank from move 2 missionaries~%")
		(setf done (right-bank (- m 2) c))
	)
	(when done (return-from left-bank 1))

	;move 2 cannibals
	(when	(and 	(>= (- c 2) 0) (<= (+ r-cann 2) *cannibals*) 
			(or (>= m (- c 2))(= m 0))
			(or (>= r-miss (+ r-cann 2)) (= r-miss 0))
		)
		(format t "To right bank from move 2 cannibals~%")
		(setf done (right-bank m (- c 2)))
	)

	(when done (return-from left-bank 1))


	;move 1 of each
	(when	(and	(>= (- m 1) 0) (>= (- c 1) 0) 
			(<= (+ r-miss 1) *missionaries*) (<= (+ r-cann 1) *cannibals*)
		)
		(format t "To right bank from move 1 of each~%")
		(format t "fail-count = " *fail-count* "-%")
		(when (>= *fail-count* 5) (return-from left-bank nil))
		(setf done(right-bank (- m 1) (- c 1)))
	)
	(when done (return-from left-bank 1))

	;move 1 missionaries
	(when	(and 	(>= (- m 1) 0) (<= (+ r-miss 1) *missionaries*) 
			(or (>= (- m 1) c) (= (- m 1) 0)) (>= (+ r-miss 1) r-cann)
		)
		(format t "To right bank from move 1 missionaries~%")
		(setf done (right-bank (- m 1) c))
	)
	(when done (return-from left-bank 1))

	;move 1 cannibals
	(when	(and 	(>= (- c 1) 0) (<= (+ r-cann 1) *cannibals*) 
			(or (>= m (- c 1))(= m 0))
			(or (>= r-miss (+ r-cann 1)) (= r-miss 0))
		)
		(format t "To right bank from move 1 cannibals~%")
		(setf done (right-bank m (- c 1)))
	)
	(when done (return-from left-bank 1))

	(format t "No move found from left.  Exiting.~%")
	(return-from left-bank NIL)
)

#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||##||
|	RIGHT-BANK 
|
|	Authors-	Jason Anderson and Johnathan Ackerman
|
|	Description-	This function determins what cases are valid and calls
|				left-bank to send the boat across the river.
|
|	Param(in)-	cann - the number of flesh eating heathens starting on the 
|							left bank hoping for a quick religious meal
|	Param(in)-	miss - the number of religious folk attempting to "save" 
|							flesh eating heathens
|
|	returns t-  the right bank returned true meaning a solution has been found
|
|	returns nil- no possible solution
|##|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
(defun right-bank (miss cann)
	(setf m miss)
	(setf c cann)
	(setf r-miss (- *missionaries* miss))
	(setf r-cann (- *cannibals* cann))
	(setf done NIL)
	(format t "m: ~S~%c: ~S~%r-miss: ~S~%r-cann: ~S~%" m c r-miss r-cann)
	(when (and (= miss 0) (= cann 0)) (return-from right-bank 1))

	;move 1 missionary back
	(when 	(and	(>= *missionaries* m) (>= (- r-miss 1) 0)
			(>= (+ m 1) c) (or (>= (- r-miss 1) r-cann)(= r-miss 0))
		)
		(format t "To left bank from move 1 missionary back~%")
		(setf *fail-count* 0)
		(setf done (left-bank (+ m 1) c))
	)
	(when done (return-from right-bank 1))

	;move 1 cannibal back
	(when	(and	(>= *cannibals* (+ c 1)) (>= (- r-cann 1) 0)
			(or (>= m (+ c 1)) (= m 0)) 
			(or (>= r-miss (- r-cann 1))(= r-miss 0))
		)
		(format t "To left bank from move 1 cannibal back~%")
		(setf *fail-count* 0)
		(setf done (left-bank m (+ c 1)))
	)
	(when done (return-from right-bank 1))

	;move 1 of each back
	(when	(and	(>= *missionaries* (+ m 1)) (>= *cannibals* (+ c 1))
			(>= (- r-miss 1) 0) (>= (- r-cann 1) 0)
		)
		(format t "To left bank from move 1 of each back~%")
		(incf *fail-count*)
		(when (>= *fail-count* 5) (return-from right-bank nil))
		(setf done (left-bank (+ m 1) (+ c 1)))
	)
	(when done (return-from right-bank 1))
	(format t "No move found from right.  Exiting.~%")
	(return-from right-bank NIL)
)

;Run missionaries and cannibals automatically upon loading file
(main)
