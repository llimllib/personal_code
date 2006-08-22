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


(defun sentence() (append(noun-phrase)(verb-phrase)))

(defun noun-phrase()(append (Article) (Adj*) (Noun) (PP*))) 

(defun verb-phrase()(append(Verb)(noun-phrase)))

(defun Article ()(one-of'(the a)))

(defun Noun ()(one-of'(man ball woman table)))

(defun Verb ()(one-of	'(hit took saw liked)))

(defun Adj* ()
    (if (= (random 2) 0) nil
   	(append (Adj) (Adj*))))

(defun PP* ()
(if (random-elt '(t nil)) 
    	(append (PP) (PP*)) nil))

(defun PP()(append (Prep) (noun-phrase)))

(defun Adj()(one-of '(big little blue green adiabatic))) 

(defun Prep()(one-of '(to in by with on)))


(defun one-of (set)
"Pick one element of set, and make a list of it." 
(list (random-elt set)))

(defun random-elt (choices)
"Choose an element from a list at random." 
(elt choices (random (length choices))))


