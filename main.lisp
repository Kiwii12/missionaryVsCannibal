;Global Variables
(defvar *missionaries*)
(defvar *cannibals*)

;main program
(defun m-c ( )
;if args not equal to 2 return
	(cond
		((= (length *args*) 2) 
		;initializes globals
			(setf *missionaries* (parse-integer (car *args*)) 
			*cannibals* (parse-integer (cdr *args*))) 
		;or print usage
		(t
			(format t "~%Missionary vs Cannibal problem~%")
			(format t "---------- -- -------- -------~%~%")
			(format t "Usage: (m-c missionaries cannibals)~%")
			(format t "	missionaries - number of missionaries trying to cross~%")
			(format t "	cannibals    - number of cannibals trying to cross~%~%")
			(error "Incorrect number of command line arguments~%~%")
		)
	)
	
;initializes globals
	(setf *missionaries* (parse-integer (car *args*)) 
			*cannibals* (parse-integer (cdr *args*)))


;if cannibals greater than missionaries return
	(cond
		((< (car args) (cdr args)) (error 
					"The missionaries have been eaten")) 
	)

;print out top of table
	(format t 
	"left bank      right bank      canoe      last move
	---------      ----------      -----      ---------"
	)

	(setf searchInfo (*missionaries* *cannibals* nil nil))
;start depth first search on successors using args as start position
	(dfs (searchInfo))


;return
)


(defun dfs (searchInfo)
;determine potential paths
(potentialPath ?)

(setf searchInfo () ? ?)
;print out current state and previous action here


;run dfs on all items in path list return success if finished

)


(defun potentialPath (?)
;must ignore paths that result in negative numbers

;must ignore paths that result in more cannibals than missionaries

;must allow cannibals if missionaries = 0

;must ignore paths that reach an already achieved state

;return list of whats left
)