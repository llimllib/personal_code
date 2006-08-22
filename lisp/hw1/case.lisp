;; case? takes an argument s of type string. It returns
;; "Upper" if the string is in all caps, "Lower" if it
;; is all lowercase, and "Mixed" if it has some of both
;; uppercase and lowercase letters.

;; If the argument is not of type string, an error is 
;; produced. This is because I have been unable to 
;; determine the return mechanism in Lisp. I could wrap 
;; the whole function in an "(if(stringp s" statement,
;; but that would be indescribably ugly, so I chose not to.

(defun case? (s)
  (setq isLower nil)
  (setq isUpper nil)
  (loop for x from 0 to (1- (length s)) do
    (if (lower-case-p (char s x))
      (setq isLower t)
      (setq isUpper t)
    )
  )
  (cond 
    ((eq isLower isUpper) (setq s (string "Mixed")))
    ((eq t isLower) (setq s (string "Lower")))
    ((eq t isUpper) (setq s (string "Upper")))
  )
)