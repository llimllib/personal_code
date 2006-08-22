;Solution for the missionaries and cannibals problem.
;
;Definition of state:
;    ((#cannibals #missionaries boatlocation) (moves))
;  where #cannibals = number of cannibals on starting shore
;        #missionaries = number of missionaries on starting shore
;        boatlocation = - for starting shore
;		        + for opposite shore
;        moves = list of operators C2, C1, M2, M1, E1


;Global variables for initial state and goal state
(setq initial '((3 3 -) ()))
(setq goal '(0 0 +))

;Returns whether people are added or subtracted from starting shore (i.e.
;boatlocation
(defun op-state (state)
	(car (last (car state))))

;Returns #cannibals in state
(defun cannibal (state)
	(caar state))

;Returns #missionaries in state
(defun missionary (state)
	(cadar state))

;Returns boatlocation in state (identical to op-state)
(defun boat (state)
	(op-state state))

;Switches boat location to opposite shore or starting shore
(defun switch (b)
	(if (equal b '+) '- '+))

;Returns list of moves for state
(defun moves (state)
	(cadr state))

;Returns a list of all possible children given state.
(defun apply-ops (state)
  (list (list (list (eval (list (op-state state) (cannibal state) 2))
		    (missionary state)
		    (switch (boat state)))
	      (cons 'c2 (moves state)))	    ;Move 2 cannibals
	(list (list (cannibal state)
		    (eval (list (op-state state) (missionary state) 2))
		    (switch (boat state)))
	      (cons 'm2 (moves state)))     ;Move 2 missionaries
	(list (list (eval (list (op-state state) (cannibal state) 1))
		    (missionary state)
		    (switch (boat state)))
	      (cons 'c1 (moves state)))      ;Move 1 cannibal
	(list (list (cannibal state)
		    (eval (list (op-state state) (missionary state) 1))
		    (switch (boat state)))
	      (cons 'm1 (moves state)))      ;Move 1 missionary
	(list (list (eval (list (op-state state) (cannibal state) 1))
		    (eval (list (op-state state) (missionary state) 1))
		    (switch (boat state)))
	      (cons 'e1 (moves state)))      ;Move 1 missionary and 1 cannibal
))


;Return nil if the state is not legal.  Otherwise return state.  Tests for legal
;state.  All of the following must apply:
;	- #missionaries >= 0
;	- #cannibals >=0
;	- #missionaries <= 3
;	- #cannibals <= 3
;	- One of the following:
;		- #missionaries >= #cannibals on both sides
;		- #missionaries = 0 on either side
;	- Current state is not initial state
;	- Current move does not repeat previous move in state
(defun testp (state)
	(if (and (>= (missionary state) 0)
		 (>= (cannibal state) 0)
		 (<= (missionary state) 3)
		 (<= (cannibal state) 3)
		 (or (and (>= (missionary state) (cannibal state))
		          (>= (- 3 (missionary state)) (- 3 (cannibal state))))
		     (equal (missionary state) 3)
		     (equal (missionary state) 0))
		 (not (equal (car state) (car initial)))
		 (not (equal (car (moves state)) (cadr (moves state)))))
	    state))
		

;Return a list of legal children for state.  Uses testp to determine legality
;of children. 
(defun expand (state)
	(remove 'nil (mapcar #'testp (apply-ops state))))


;Return t if state is the goal
(defun check (state)
	(equal (car state) goal))


;Uses breadth-first search to solve missionaries and cannibals problem.
(defun bfs (q)
  (if 
    ;;if the list is null, return nil
    (null q) nil
    ;;if we're at the goal, return the moves you took to get there
    (if (check (car q)) (reverse (moves (car q)))
    ;;otherwise, search on the list after making the first move
    (bfs (append (cdr q) (expand (car q)))))))



;Uses depth-first search to solve missionaries and cannibals problem.
(defun dfs (q)
        (if (null q) nil
            (if (check (car q)) (reverse (moves (car q)))
                (dfs (append (expand (car q)) (cdr q))))))    

