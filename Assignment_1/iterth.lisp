#!/usr/bin/env clisp
;;;;
;;;; Towers of Hanoi - Recursive Solution
;;;; Mike Lane
;;;; CS 541
;;;;
;;;; I've made this program a CLI script that takes its parameters as space-
;;;; separated CLI arguments. There is a basic help function that informs the
;;;; user on the usage in case they get it incorrect.
;;;;


;;; This tweak-text function came from the book Land of Lisp. It exists to make
;;; the output look nice
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
    (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
          ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
          ((eq item #\") (tweak-text rest caps (not lit)))
          (lit (cons item (tweak-text rest nil lit)))
          ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
          (t (cons (char-downcase item) (tweak-text rest nil nil)))))))


;;; This game-print function came from the book Land of Lisp. It replaces the
;;; print function (and its variants) for outputting messages to the screen.
(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (prin1-to-string lst))
                                     'list)
                             t
                             nil)
                 'string))
  (fresh-line))


;;; This is my own work. This is the function that drives the movement of the
;;; disks from source to destination.
(defun legal-top-disk-move (src dest s d)
  (let ((p1top (pop src))
        (p2top (pop dest)))
    (cond ((eq p1top NIL)
           (push p2top src)
           (game-print `(moving disk ,p2top from post ,d to post ,s)))
          ((eq p2top NIL)
           (push p1top dest)
           (game-print `(moving disk ,p1top from post ,s to post ,d)))
          ((> p1top p2top)
           (push p1top src)
           (push p2top src)
           (game-print `(moving disk ,p2top from post ,d to post ,s)))
          (t
            (push p2top dest)
            (push p1top dest)
            (game-print `(moving disk ,p1top from post ,s to post ,d))))
    `(,src ,dest)))


;;; This is my own work. This generates a a list (1 2 3 ... n)
(defun number-sequence (n)
  (loop for x from 1 to n
        collect x))


;;; This is my own work. If n is 1, then move the disk from the post it is on to
;;; the desired post. Otherwise, move all the disks less than n to the aux post
;;; move disk n to the desired final post, and then move all the smaller disks
;;; to the desired final post. Recursion takes care of the mess beautifully.
(defun towers-of-hanoi (n from to aux)
  (if (evenp n)
    (let ((tmp to))
      (setf to aux)
      (setf aux tmp)))
  (let ((SRC (number-sequence n))
        (AUXI ())
        (DEST ()))
    (cond ((eq n 1) (legal-top-disk-move SRC DEST from to))
          (t (loop for i from 1 to (1- (expt 2 n)) do
                   ;(print `(,SRC ,AUXI ,DEST)
                   (cond ((eq 1 (mod i 3))
                          (setf ret (legal-top-disk-move SRC DEST from to))
                          (setf SRC (car ret))
                          (setf DEST (cadr ret)))
                         ((eq 2 (mod i 3))
                          (setf ret (legal-top-disk-move SRC AUXI from aux))
                          (setf SRC (car ret))
                          (setf AUXI (cadr ret)))
                         ((eq 0 (mod i 3))
                          (setf ret (legal-top-disk-move AUXI DEST aux to))
                          (setf AUXI (car ret))
                          (setf DEST (cadr ret)))))))))


;;; Handles parsing the command line arguments. This is not very sophisticated.
;;; It will get tripped up on bad inputs, but it should suffice for homework.
;;; Hopefully.
(if (eq 4 (list-length ext:*args*))
  ;; Only try to run TOH if there are 4 args
  (let ((n (parse-integer (car ext:*args*)))
        (from (cadr ext:*args*))
        (to (caddr ext:*args*))
        (aux (cadddr ext:*args*)))
    (towers-of-hanoi n from to aux))
  ;; Otherwise print out a help message
  (progn (princ "usage: clisp recth.lisp n from to aux")
         (princ #\newline)
         (princ "      n: number of disks")
         (princ #\newline)
         (princ "   from: name of starting peg")
         (princ #\newline)
         (princ "     to: name of destination peg")
         (princ #\newline)
         (princ "    aux: name of auxilery peg")
         (princ #\newline)
         (princ "example: clisp recth.lisp 5 A C B")))


