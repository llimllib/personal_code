;;; File: Grammar1a.lisp

;;; A Simple Grammar
;;; Now with Strings! Teach your kid english in style with our perfect
;;; grammar generator.

;;; grammar rules:

;;;   Sentence => Noun-Phrase + Verb-Phrase 
;;;   Noun-Phrase => Article + Adj* + Noun + PP*
;;;   Verb-Phrase => Verb + Noun-Phrase 
;;;   Article => the, a,. . .
;;;   Noun => man, ball, woman, table ... 
;;;   Verb => hit, took, saw, liked ...
;;;   PP* => 0, PP + PP
;;;   PP => Prep + Noun-Phrase Adj => big, little, blue, green, . . . 
;;;   Prep => to, in, by, with, . . .

;; accepts no arguments, returns nothing. It prints out a question.
;; This should be the only function called from this file - the rest
;; are helper functions.
(defun question() 
  (format t "~a ~a ~a?" (question-phrase) (noun-phrase) (verb-phrase)))

(defun noun-phrase()
  (string-trim " " (concatenate 'string (Article) " " (Adj) " " (Noun) " " (PP*))))

(defun verb-phrase()
  (format nil "~a ~a" (Verb) (noun-phrase)))

(defun question-phrase() 
  (format nil "~a ~a" (question-word)(question-article)))

(defun question-word() 
  (format nil "~a" (one-of '("Why" "Who" "Where" "What" "When"))))

(defun question-article() 
  (format nil "~a" (one-of '("did" "is"))))

(defun Article ()
  (format nil "~a" (one-of '("the" "a"))))

(defun Noun ()
  (format nil "~a" (one-of '("man" "ball" "woman" "table"))))

;; Changed to present-tense forms
(defun Verb ()
  (format nil "~a" (one-of '("hit" "take" "see" "like"))))

(defun PP* ()
  (if (random-elt '(t nil)) 
    (concatenate 'string (PP) " " (PP*)) nil))

(defun PP()
  (format nil "~a ~a" (Prep) (noun-phrase)))

(defun Adj()
  (format nil "~a" (one-of '("big" "little" "blue" "green" "adiabatic"))))

(defun Prep()
  (format nil "~a" (one-of '("to" "in" "by" "with" "on"))))

(defun one-of (set)
"Pick one element of set." 
  (random-elt set))

(defun random-elt (choices)
"Choose an element from a list at random." 
  (elt choices (random (length choices))))


