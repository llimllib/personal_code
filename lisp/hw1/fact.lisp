;; fact takes an argument of type number and returns
;; its factorial.

(defun fact (num)
  (cond
   ((not (numberp num)) nil)
   ((zerop num) 1)
   ((> num 0) (* num (fact (1- num))))
  )
)
