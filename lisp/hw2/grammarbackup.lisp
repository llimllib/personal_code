;;; File: Grammar1a.lisp

;;; A Simple Grammar
;;; Noun-Phrase made more complicated in this version

;;; grammar rules:

;;;   Sentence => Noun-Phrase + Verb-Phrase 
;;;   Noun-Phrase => Article + Adj* + Noun + PP*
;;;   Verb-Phrase => Verb + Noun-Phrase 
;;;   Article => the, a,. . .
;;;   Noun => man, ball, woman, table ... 
;;;   Verb => hit, took, saw, liked ...
;;;   Adj* => 0, Adj + Adj
;;;   PP* => 0, PP + PP
;;;   PP => Prep + Noun-Phrase Adj => big, little, blue, green, . . . 
;;;   Prep => to, in, by, with, . . .


(defun question() 
  ;;(concatenate 'string (question-phrase)(noun-phrase)(verb-phrase)"?"))
  (concatenate 'string (question-phrase) "_" (noun-phrase) "+" (verb-phrase)))

(defun noun-phrase()(concatenate 'string (Article) " " (Adj) " " (Noun) " " (PP*))) 

(defun verb-phrase()(concatenate 'string (Verb)(noun-phrase)))

(defun question-phrase() (format nil "~a ~a" (question-word)(question-article)))

(defun question-word() 
  (format nil "~a" (one-of '("Why" "Who" "Where" "What" "When"))))

(defun question-article() (format nil "~a" (one-of '("did" "is"))))

(defun Article ()(format nil "~a" (one-of '("the" "a"))))

(defun Noun ()(format nil "~a" (one-of '("man" "ball" "woman" "table"))))

;; Changed to present-tense forms
(defun Verb ()(format nil "~a" (one-of '("hit" "take" "see" "like"))))

(defun PP* ()
(if (random-elt '(t nil)) 
    	(concatenate 'string (PP) " " (PP*)) nil))

(defun PP()(concatenate 'string (Prep) " " (noun-phrase)))

(defun Adj()
  (format nil "~a" (one-of '("big" "little" "blue" "green" "adiabatic"))))

(defun Prep()(format nil "~a" (one-of '("to" "in" "by" "with" "on"))))

(defun one-of (set)
"Pick one element of set, and make a list of it." 
  (random-elt set))

(defun random-elt (choices)
"Choose an element from a list at random." 
(elt choices (random (length choices))))


