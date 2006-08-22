;; nqueens problem

(defparameter *aima-file* "e:/my_documents/mylisp/book_code/aima.lisp")

(load *aima-file*)

(aima-load 'all)

(defun solve-8-queens ()
  (solve (make-nqueens-problem) 'csp-backtracking-search)
  
;; How solve works here:
;; solve in this case is a good deal more complicated than in the cannibals problem
;; for two reasons. First, the nqueens-problem structure is a subtype of csp-problem,
;; which is in turn a subtype of problem. This inheritance adds a couple of slots
;; crucial to constraint-satisfaction problems - predicates for forward checking and
;; legality checking, as well as a slot to hold the variable to be used next. This time,
;; the algorithm called by solve does not use general-search, as that is not well suited
;; to csp problems. Instead, it implements a fairly straightforward algorithm:
;; csp-backtracking-search:
;;   make-initial-queue
;;   loop
;;     if queue is empty, return nil
;;     node <- first node in queue
;;     if node is legal
;;       if node is goal return node
;;       else expand node
