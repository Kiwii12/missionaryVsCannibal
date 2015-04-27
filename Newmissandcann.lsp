;Global Variables
(defvar *missionaries*)	;Number of missionaries
(defvar *cannibals*)	;Number of cannibals
(defvar *solution*)	;solution path
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
	(setf *solution* '(0))
	(setf *fail-count* 0)

	(cond 
		((left-bank *missionaries* *cannibals*)
			(format t "left bank right bank canoe next move~%")
			(format t "--------- ---------- ----- ---------~%")
			(pop *solution*)
			(print-solution)
		)
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
|	Description-	This function determines what cases are valid and calls
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

	;move 2 missionaries
	(when	(and 	(>= (- m 2) 0) (<= (+ r-miss 2) *missionaries*) 
			(or (>= (- m 2) c) (= (- m 2) 0)) (>= (+ r-miss 2) r-cann)
		)
		;(format t "To right bank from move 2 missionaries~%")
		(format-solution m c 0 0)
		(setf done (right-bank (- m 2) c))
	)
	(when done (return-from left-bank 1))

	;move 2 cannibals
	(when	(and 	(>= (- c 2) 0) (<= (+ r-cann 2) *cannibals*) 
			(or (>= m (- c 2))(= m 0))
			(or (>= r-miss (+ r-cann 2)) (= r-miss 0))
		)
		;(format t "To right bank from move 2 cannibals~%")
		(format-solution m c 0 1)
		(setf done (right-bank m (- c 2)))
	)
	(when done (return-from left-bank 1))


	;move 1 of each
	(when	(and	(>= (- m 1) 0) (>= (- c 1) 0) 
			(<= (+ r-miss 1) *missionaries*) (<= (+ r-cann 1) *cannibals*)
		)
		;(format t "To right bank from move 1 of each~%")
		;(format t "fail-count = " *fail-count* "~%")
		(when (>= *fail-count* 5) (return-from left-bank nil))
		(format-solution m c 0 2)
		(setf done(right-bank (- m 1) (- c 1)))
	)
	(when done (return-from left-bank 1))

	;move 1 missionaries
	(when	(and 	(>= (- m 1) 0) (<= (+ r-miss 1) *missionaries*) 
			(or (>= (- m 1) c) (= (- m 1) 0)) (>= (+ r-miss 1) r-cann)
		)
		;(format t "To right bank from move 1 missionaries~%")
		(format-solution m c 0 3)
		(setf done (right-bank (- m 1) c))
	)
	(when done (return-from left-bank 1))

	;move 1 cannibals
	(when	(and 	(>= (- c 1) 0) (<= (+ r-cann 1) *cannibals*) 
			(or (>= m (- c 1))(= m 0))
			(or (>= r-miss (+ r-cann 1)) (= r-miss 0))
		)
		;(format t "To right bank from move 1 cannibals~%")
		(format-solution m c 0 4)
		(setf done (right-bank m (- c 1)))
	)
	(when done (return-from left-bank 1))

	;(format t "No move found from left.  Exiting.~%")
	(return-from left-bank NIL)
)

#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||##||
|	RIGHT-BANK 
|
|	Authors-	Jason Anderson and Johnathan Ackerman
|
|	Description-	This function determines what cases are valid and calls
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
	(when (and (= miss 0) (= cann 0)) (format-solution m c 1 10) 
		(return-from right-bank 1))

	;move 1 missionary back
	(when 	(and	(>= *missionaries* m) (>= (- r-miss 1) 0)
			(>= (+ m 1) c) (or (>= (- r-miss 1) r-cann)(= r-miss 0))
		)
		;(format t "To left bank from move 1 missionary back~%")
		(setf *fail-count* 0)
		(format-solution m c 1 5)
		(setf done (left-bank (+ m 1) c))
	)
	(when done (return-from right-bank 1))

	;move 1 cannibal back
	(when	(and	(>= *cannibals* (+ c 1)) (>= (- r-cann 1) 0)
			(or (>= m (+ c 1)) (= m 0)) 
			(or (>= r-miss (- r-cann 1))(= r-miss 0))
		)
		;(format t "To left bank from move 1 cannibal back~%")
		(setf *fail-count* 0)
		(format-solution m c 1 6)
		(setf done (left-bank m (+ c 1)))
	)
	(when done (return-from right-bank 1))

	;move 1 of each back
	(when	(and	(>= *missionaries* (+ m 1)) (>= *cannibals* (+ c 1))
			(>= (- r-miss 1) 0) (>= (- r-cann 1) 0)
		)
		;(format t "To left bank from move 1 of each back~%")
		(incf *fail-count*)
		(when (>= *fail-count* 5) (return-from right-bank nil))
		(format-solution m c 1 7)
		(setf done (left-bank (+ m 1) (+ c 1)))
	)
	(when done (return-from right-bank 1))

	;(format t "No move found from right.  Exiting.~%")
	(return-from right-bank NIL)
)

#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||##||
|	FORMAT-SOLUTION
|
|	Authors-	Jason Anderson and Johnathan Ackerman
|
|	Description-	This function formats the solution for when the print 
|				solution is called it knows where to find the 
|				information to print out
|
|	Param(in)-	cann - the number of flesh eating heathens starting on the 
|							left bank hoping for a quick |							religious meal
|
|	Param(in)-	miss - the number of religious folk attempting to "save" 
|							flesh eating heathens
|
|	Param(in)-	right - A boolean value representing which side of the river
|					the boat is on
|
|	Param(in)-	move - The code for print-solution for which move was last made
|
|	Param(out)-	*solution* - where the path information is stored
|
|	returns t-  returns not NIL to get back into the body of the code
|##|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
;Store the solution path
(defun format-solution (miss cann right move)
	(setf path 0)
	(setf path (list 0 miss cann 
			(- *missionaries* miss) (- *cannibals* cann) right move))
	(nconc *solution* path)
	(return-from format-solution 1)
)

#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||##||
|	PRINT-SOLUTION
|
|	Authors-	Jason Anderson and Johnathan Ackerman
|
|	Description-	This function prints out the solution formatted nicely
|
|	Param(in)-	*solution* - Global varibal holds the path for the solution
|
|	returns t-  returns nil when there is no more moves in the *solution* varibal
|##|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
;format the path
(defun print-solution ()
	(setf end (pop *solution*))
	(when (equalp end NIL) (return-from print-solution NIL))
	(format t "~S M, ~S C  ~S M, ~S C   "
		(pop *solution*)(pop *solution*)(pop *solution*)(pop *solution*))
	(setf boat (pop *solution*))
	(when (= boat 0) (format t "Left  "))
	(when (= boat 1) (format t "Right "))
	(setf move (pop *solution*))
	(when (= move 0) (format t "Move 2 missionaries from left to right~%"))
	(when (= move 1) (format t "Move 2 cannibals from left to right~%"))
	(when (= move 2) (format t "Move 1 of each from left to right~%"))
	(when (= move 3) (format t "Move 1 missionary from left to right~%"))
	(when (= move 4) (format t "Move 1 cannibal from left to right~%"))
	(when (= move 5) (format t "Move 1 missionary from right to left~%"))
	(when (= move 6) (format t "Move 1 cannibal from right to left~%"))
	(when (= move 7) (format t "Move 1 of each from right to left~%"))
	(when (= move 10) (format t "Success!~%"))
	(print-solution)
)

;Run missionaries and cannibals automatically upon loading file
(main)
