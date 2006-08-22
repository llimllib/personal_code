;; my-third takes a list l and returns the third
;; value from this list.
;;
;; If the argument is not of type list, an error is 
;; produced. This is because I have been unable to 
;; determine the return mechanism in Lisp. I could wrap 
;; the whole function in an "(if(listp s" statement,
;; but that would be indescribably ugly, so I chose not to.

(defun my-third (l)
  (setq iter 0)
  (loop for iter from 0 to 1 do
    (setq l (rest l))
  )
  (setq l (first l))
)
