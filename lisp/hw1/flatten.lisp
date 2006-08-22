;; flatten-tree takes a list l and flattens it
;; into one dimension.
;;
;; Thanks to 
;; http://theory.lcs.mit.edu/~meyer/6001/FT97/sections/section7/sect-11-21.txt
;; for the algorithm (in scheme) and http://www.google.com
;; for being the best search engine around.

(defun flatten-tree (l)
  (cond 
    ((null l) nil)
    ((atom l) (list l))
    (t
      (append (flatten (first l))
              (flatten (rest l)))
    )
  )
)

