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


;;; This is my own work. If n is 1, then move the disk from the post it is on to
;;; the desired post. Otherwise, move all the disks less than n to the aux post
;;; move disk n to the desired final post, and then move all the smaller disks
;;; to the desired final post. Recursion takes care of the mess beautifully.
(defun towers-of-hanoi (n from to aux)
  (if (eq n 1)
    ; Base Case
    (game-print `(moving disk ,n from post ,from to post ,to))
    ; Recursive steps
    (progn (towers-of-hanoi (1- n) from aux to)  ; All above n from current to aux
           (game-print `(moving disk ,n from post ,from to post ,to))
           (towers-of-hanoi (1- n) aux to from))))  ; All above n from aux to dest


;;; Output a helpful CLI message
(defun print-help-message ()
  (princ
"
usage: clisp recth.lisp n from to aux

       n: number of disks
    from: name of starting peg
      to: name of destination peg
     aux: name of auxiliary peg

 example: clisp recth.lisp 5 A C B

"))


;;; Handles parsing the command line arguments. This is not very sophisticated.
;;; It will get tripped up on bad inputs, but it should suffice for homework.
;;; Hopefully.
(if (eq 4 (list-length ext:*args*))
  ;; Only try to run TOH if there are 4 args
  (let ((n (handler-case (parse-integer (car ext:*args*))
             (error (c)
                    (print-help-message)
                    (ext:exit 1))))
        (from (cadr ext:*args*))
        (to (caddr ext:*args*))
        (aux (cadddr ext:*args*)))
    (towers-of-hanoi n from to aux))
  ;; Otherwise print out a help message
  (print-help-message))

