(defparameter *aima-file* "e:/my_documents/mylisp/book_code/aima.lisp")

(load *aima-file*)

(aima-load 'all)

(defun cannibals ()
  ;;Uncomment this for a bfs in the strict sense (no repeat checking)...
  ;;it will check 11877 nodes versus 14 with repeat checking
  ;;(solve (make-cannibal-problem) 'breadth-first-search)
  
  ;;bfs solution with repeat checking:
  (solve (make-cannibal-problem) 'no-duplicates-breadth-first-search))

;; How solve works in this instance:
;; creates a cannibal-problem structure with the default values. Cannibal-problem
;; is a subtype of structure problem. It then tells solve to work on 
;; cannibal-problem with the bfs algorithm, ignoring duplicates. To do this,
;; solve calls the function with the name of the algorithm you've passed it and
;; sends it the problem structure. The search function uses general-search to
;; search the nodes, returns them to solve, and solve calls print-solution to
;; print the results. And there was much rejoicing.