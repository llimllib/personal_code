;; all-odds takes an argument of type list and returns
;; a list containing all of the odd numbers from the
;; argument.
;;
;; If the argument is not of type list, an error is 
;; produced. This is because I have been unable to 
;; determine the return mechanism in Lisp. I could wrap 
;; the whole function in an "(if(listp s" statement,
;; but that would be indescribably ugly, so I chose not to.

(defun all-odds (l)
  (setq results (list))
  (loop for x from 0 to (1- (length l)) do
    (if (oddp (nth x l))
      (setq results (append results (list (nth x l))))
    )
  )
  (setq l results)
)
