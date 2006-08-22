;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Wicked Hoagie
;;; Bryan Brook
;;; John Quinn
;;; Bill Mill
;;; Lisp Homework #7
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;
;; Chapter 7
;;;;;;;;;;;;;;;

;;;;;;
;; Problem 1
;;;;;;

>(defun putprop (name val prop)
>    (setf (get name prop) val))

PUTPROP

>(putprop x 'blue 'color)

COLOR

>(get x 'color)

BLUE

;;;;;;;
;; Problem 2
;;;;;;;

>(defun cheap-car (cars)
>    (cond ((null (cdr cars)) (car cars))
>          ((< (get (car cars) 'cost) (get (cheap-car (cdr cars)) 'cost)) (car cars))
>          (t (cheap-car (cdr cars)))))

CHEAP-CAR

>(defun cheapest-car-style (cars)
>    (get (cheap-car cars) 'title))

CHEAPEST-CAR-STYLE

>*cars*

(TOYOTA PLYMOUTH BMW BENZ)

>(cheapest-car-style *cars*)

"Toyota Tercel"

>(cheapest-car-style nil)

NIL

>(cheapest-car-style '(bmw benz bmw))

"BMW 320i"

;;;;;;;
;; Problem 3
;;;;;;;

CL-USER 1 > (defun mark1 (lst value)
              (cond 
               ((null lst) nil)
               (t(mark1 (cdr lst) value)))
                (setf(get (car lst) 'lsted) value))
MARK1

CL-USER 2 > (defun setinter (lst1 lst2)
       (mark1 lst1 't)
       (cond 
           ((null lst2) nil)                                                                         
           ((equal (get (car lst2) 'lsted) 't)
            (setf (get (car lst2) 'lsted) nil) 
            (cons (car lst2) (setinter nil (cdr lst2))))
          (t (setinter nil (cdr lst2))))) 
SETINTER

CL-USER 3 > (setinter '(a b c) '(a a c))
(A C)

CL-USER 4 >  (mark1 '(a b c) nil) 
NIL


;;;;;;;;;;;;;
;; Chapter 8
;;;;;;;;;;;;;

;;;;;;;
;; Problem 1
;;;;;;;

>(eval (list 'car '(cdr '(b c))))

C

>(eval (list 'car '(cdr ''(b c))))

(B C)

>(eval (cons 'cdr '('(a b c))))

(B C)

>(apply 'cdr '((a b c)))

(B C)

>(mapcar 'list '(a b) '(c d))

((A C) (B D))

;;;;;;;
;; Problem 2
;;;;;;;

>(defun make-assoc-list (lst1 lst2)
>    (mapcar 'list lst1 lst2))

MAKE-ASSOC-LIST

>(make-assoc-list '(a b c d) '(1 2 3 4))

((A 1) (B 2) (C 3) (D 4))

>(make-assoc-list nil '(1 2))

NIL

;;;;;;
;; Problem 3
;;;;;;

>(defun my-mapcar (func lst1)
>    (cond ((or (not (functionp func)) (null lst1) (not (listp lst1))) nil)
>          ((null (cdr lst1)) (list (apply func lst1)))
>          ((cons (funcall func (car lst1)) (my-mapcar func (cdr lst1))))))

MY-MAPCAR

>(my-mapcar #'1+ nil)

NIL

>(my-mapcar nil '(2 3 4))

NIL

>(my-mapcar #'1+ '(2 3 4))

(3 4 5)

;;;;;;;
;; Problem 4
;;;;;;;

>(defun transpose(l)
    (cond ((or (null l) (not (listp l))) nil)
          (t (apply 'mapcar (cons 'list l)))))

TRANSPOSE

>(transpose '((a b c) (1 2 3) (d e f)))

((A 1 D) (B 2 E) (C 3 F))

>(transpose '((a b c d e) (1 2 3 4 5) (f g h i j k)))

((A 1 F) (B 2 G) (C 3 H) (D 4 I) (E 5 J))

>(transpose nil)

nil

;;;;;;;
;; Problem 5
;;;;;;;

>(defun repeat (elt n)
> ;; Return a list containing n copies of elt
>    (cond 
>          ((= 0 (1- n)) (list elt))
>          (t (cons elt (repeat elt (1- n))))))

REPEAT

>(defun put-not-found (lst)
> ;; mark all elements of lst as not found
>    (cond ((null lst) nil)
>          (t (mapcar #'putprop lst (repeat 'no (length lst)) 
>             (repeat 'found (length lst))))))

PUT-NOT-FOUND

>(defun is-found (elt)
> ;; mark elt as found
>    (putprop elt 'yes 'found))

IS-FOUND

>(defun return-found (lst)
> ;; return all elements of lst with 'found set to 'yes
>    (cond ((null lst) lst)
>          ((eq (get (car lst) 'found) 'yes) (cons (car lst) (return-found (cdr lst))))
>          (t (return-found (cdr lst)))))

RETURN-FOUND

>(defun inter (lst1 lst2)
> ;; return an intersection of sets lst1 and lst2
>    (put-not-found lst1)
>    (mapcar #'is-found lst2)
>    (return-found lst1))

INTER

>(inter nil nil)

NIL

>(inter '(a b c) nil)

NIL

>(inter '(a b c) '(b c d))

(B C)

>(inter '(a b c) '(d e f))

NIL

;;;;;;;;
;; Problem 6
;;;;;;;;

(defun truep (x) (not (null x)))

(defun exist-true? (lst)
    (eval (cons 'some '(#'truep lst))))

;;;;;;;;
;; Problem 7
;;;;;;;;

>(defun my-some (func lst)
>    (cond ((or (not (functionp func)) (not (listp lst))) nil)
>          ((null lst) lst)
>          ((eq (funcall func (car lst)) nil) (my-some func (cdr lst)))
>          (t lst)))

MY-SOME

>(my-some #'numberp nil)

NIL

>(my-some #'numberp '(1 2 3 4))

(1 2 3 4)

>(my-some #'numberp '(a b c d 2 e))

(2 E)

>(defun my-every (func lst)
>    (cond ((or (not (functionp func)) (not (listp lst))) nil)
>          ((null lst) t)
>          ((eq (funcall func (car lst)) nil) nil)
>          (t (my-every func (cdr lst)))))

MY-EVERY

>(my-every #'numberp '(1 2 3))

T

>(my-every #'numberp '(1 2 3 a))

NIL

>(my-every #'numberp '(1 2 3 a 6))

NIL

>(my-every #'numberp nil)

T

;; note: I consider this expected behavior, as this is vacuously true. see:
>(every #'numberp nil)

T

;;;;;;;;
;; Problem 8
;;;;;;;;

>(defun subset (func lst)
>    (cond ((or (not (functionp func)) (not (listp lst))) nil)
>          ((null lst) lst)
>          ((eq (funcall func (car lst)) nil) (subset func (cdr lst)))
>          (t (cons (car lst) (subset func (cdr lst))))))

SUBSET

>(subset #'numberp '(a b 2 c d 3 e f))

(2 3)

>(subset #'numberp nil)

NIL

;
