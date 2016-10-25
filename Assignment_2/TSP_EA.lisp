;;;;
;;;; Michael Lane
;;;; 11/01/2016
;;;; CS541
;;;; Lab 2
;;;;
;;;; Implements a Genetic Algorithm to determine an acceptible solution to the
;;;; traveling salesman problem.
;;;;


;;; Define the necessary variables
(defvar *num-cities* 0)
(defvar *city-array* nil)
(defvar *start-city* nil)


;;; Define a city class
(defclass city ()
  ((num
     :initarg :num
     :accessor city-num)
   (x
     :initarg :x
     :initform 0
     :accessor city-x)
   (y
     :initarg :y
     :initform 0
     :accessor city-y)))


(defun randomize-circuit (circuit start-num)
  (let ((index-of-start (position start-num circuit :test (lambda (x y) (eq x (city-num y))))))
    ;; First get the start city to index 0
    (rotatef (aref circuit index-of-start)
             (aref circuit 0))
    ;; Now do a Knuth shuffle for everything at index 1 and onwards
    (loop for i from 1 below (1- *num-cities*) do
          (rotatef (aref circuit i)
                   (aref circuit (+ i (random (- *num-cities* i))))))
    ))


(defun calc-dist (circuit)
  (print circuit))


;; Define a circuit class
(defclass circuit ()
  ((cities
     :initform (randomize-circuit (make-array *num-cities*
                                              :element-type 'city
                                              :initial-contents *city-array*)
                                  *start-city*)
     :accessor cities)
   (circuit-dist
     :initform (calc-dist ()))))


;;; Import the data from the file.
;;; File must be of the following format:
;;;
;;; <number of cities>
;;; <city number> <x-coordinate> <y-coordinate>
;;; <start city number>
;;;
;;; Where the city number, x-coord, y-coord line is repeated for each of the
;;; number of cities
;;;
(defun import-data (filename)
  (with-open-file (input filename)
    ;; Read the first line of the data file and set the value of *num-cities*
    (setf *num-cities* (read-from-string (format nil "~d~%" (read-line input nil))))

    ;; Make the array of cities by iterating over the input file. Lisp arrays have
    ;; O(1) indexing with aref whereas lists have O(n) indexing with elt, so this
    ;; is important for runtime efficiency of the GA algorithm.
    (setf *city-array*
          (make-array *num-cities*
                      :element-type 'city
                      :initial-contents
                      ;; Make a list of cities the initial array element argument
                      (loop for i from 0 below *num-cities*
                            ;; Collect a list of city objects
                            collect (eval  ; Finally evaluate the city constructor s-expression
                                      ;; Change the city constructor expression string to an s-expression
                                      (read-from-string
                                        ;; Make a string that corresponds to a city constructor expression
                                        (concatenate 'string
                                                     "(make-instance 'city "
                                                     ;; Remove the initial parenthesis
                                                     (subseq
                                                       ;; Convert the result into a string
                                                       (format nil "~s"
                                                               ;; Map the input values to the city constructor arguments
                                                               (mapcan #'list
                                                                       '(:num :x :y)
                                                                       (read-from-string
                                                                         (format nil "(~d)~%" (read-line input nil)))))
                                                       1))  ; The 2nd arg of subseq
                                        ))
                            ))
          )

    ;; Read the last line of the data file and set the value of *start-city*
    (setf *start-city* (read-from-string (format nil "~d~%" (read-line input nil))))
    ))

;;; Used for testing import-data function
(import-data "test.txt")
(print *num-cities*)
(print *start-city*)
#|
 |(loop for city across *city-array* do
 |      (print (format nil
 |                     "n: ~d coords: (~d,~d)"
 |                     (city-num city) (city-x city) (city-y city))))
 |(print (position 1 *city-array* :test (lambda (x y) (eq x (city-num y)))))
 |#
(defvar *test* (copy-city-array))
#|
 |(loop for city across *test* do
 |      (print (format nil
 |                     "n: ~d coords: (~d,~d)"
 |                     (city-num city) (city-x city) (city-y city))))
 |
 |#


;;; Class method for city that returns the euclidean distance between 2 cities
;;;
(defmethod city-distance ((a city) (b city))
  (sqrt (+
          (expt (-
                  (city-x a)
                  (city-x b))
                2)
          (expt (-
                  (city-y a)
                  (city-y b))
                2))
        ))


#|
 |;;; Used for testing the city-distance method
 |(import-data "test.txt")
 |(princ (city-distance (aref *city-array* 0)
 |                      (aref *city-array* 1)))
 |#


;;; Initialize a set of lists of n individuals drawn from an alist of city, location pairs
;;;
;(defun initialize-population (n l) ())


;;; Given a list of cities, (x_1, x_2, ..., x_n), return the total hamiltonian circuit
;;; distance from x_1 -> x_2 -> ... -> x_n -> x_1.
;;;
;(defun fitness (l) ())


;;; Select the x fittest individuals out of a list of individuals (those that have the lowest distance)
;;;
;(defun select-parents (x l) ())


;;; Swap two random values in a list
;;;
;(defun random-swap (a) ())


;;; Mate the list of the surviving parents together using a roulette wheel selection
;;; mechanism and perform a swap mutation on x% of the population
;;;
;(defun crossover-mutation (x l) ())


;;; Select parents to mate using roulette wheel selection and generate children until the
;;; population is full again.
;;;
;(defun replace-population (l) ())


;(defun run () ())
;;;;
;;;; Michael Lane
;;;; 11/01/2016
;;;; CS541
;;;; Lab 2
;;;;
;;;; Implements a Genetic Algorithm to determine an acceptible solution to the
;;;; traveling salesman problem.
;;;;


;;; Define the necessary variables
(defvar *num-cities* 0)
(defvar *city-array* nil)
(defvar *start-city* nil)
