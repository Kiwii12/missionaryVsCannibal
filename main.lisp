

;main program
(defun m-c ( args )
;if args not equal to 2 return


;if cannibals greater than missionaries return


;print out top of table

(setf searchInfo ((con args) (cdr args) nil nil))
;start depth first search on successors using args as start possition
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