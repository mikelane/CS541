;;;;
;;;; Michael Lane
;;;; 11/01/2016
;;;; CS541
;;;; Lab 2
;;;;
;;;; Implements a Genetic Algorithm to determine an acceptible solution to the
;;;; traveling salesman problem.
;;;;


;;; Define the classes

;; The circuit class. A circuit is a collection of cities, the total distance of
;; the Hamiltonian path, and the fitness of that class
(defclass circuit ()
  ((cities
     :initform nil
     :initarg :cities
     :accessor cities)
   (circuit-dist
     :initform 0
     :initarg :circuit-dist
     :accessor circuit-dist)
   (circuit-fitness
     :initform 0
     :initarg :circuit-fitness
     :accessor circuit-fitness)))


;; The city class. A city is an identifying number and an x and y coordinate on
;; a 200 x 200 grid.
;;
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


;;; Define the necessary variables
(defvar *num-cities* 0)
(defvar *city-array* nil)
(defvar *start-city* nil)
(defparameter *population-number* 100)
(defvar *population* (make-array *population-number*
                                 :element-type 'circuit
                                 :fill-pointer 0
                                 :initial-element nil))


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


;;; Take a circuit and randomize it but maintain the initial city in the ciruit
;;;
(defun randomize-circuit (circuit)
  ;; Note that the start city has already been placed at index 0 so simply do a
  ;; Knuth shuffle for everything at index 1 and onwards
  (loop for i from 1 below (1- *num-cities*) do
        (rotatef (aref circuit i)
                 (aref circuit (+ i (random (- *num-cities* i)
                                            (make-random-state t))))))
  circuit)


;;; Wrapper function for city-dist. We need this wrapper since we have to add in
;;; the value of the distance from the last city to the start city
;;;
(defun calc-dist (city-arr)
  ;; If there is 0 or 2 cities, this should return nil
  (cond ((< (length city-arr) 2) nil)
        ((eq (length city-arr) 2) (* 2 (city-dist (aref city-arr 0)
                                                  (aref city-arr 1))))
        (t (+ (city-distance (aref city-arr 0)
                             (aref city-arr (1- (length city-arr))))
              (calc-dist-rec city-arr)))
        ))


;;; The recursive function to calculate the distance. Take a city vector and add
;;; the values of the distances to the cities in the order they are stored. The
;;; wrapper prevents the case that there are less than 2 cities in an array.
;;;
(defun calc-dist-rec (city-arr)
  ;; Base case, 2 cities in the array, return their distance.
  (cond ((eq (length city-arr) 2)
         (city-distance (aref city-arr 0)
                        (aref city-arr 1)))
        ;; recursive case, return the distance of the first 2 plus the distance
        ;; of the recursive call of the list from index 1 onwards
        (t (+ (city-distance (aref city-arr 0) (aref city-arr 1))
              (calc-dist-rec (subseq city-arr 1))))
        ))


(defun calc-fitness (circuit-dist)
  (cond ((eq 0 circuit-dist) nil)
        (t (/ 1 circuit-dist))))


;;; Initialize a vector of n randomized circuits. This is the initial population
;;; for the Genetic Algorithm.
;;;
(defun initialize-population (n)
  (let ((c nil))
        ;(c-dist 0)
        ;(c-fit 0))
    (loop for i from 0 below n do
          (setf c (randomize-circuit
                    (make-array *num-cities*
                                :element-type 'city
                                :initial-contents *city-array*)))
          (setf c-dist (calc-dist c))
          (setf c-fit (calc-fitness c-dist))
          (vector-push (make-instance 'circuit
                                      :cities c
                                      :circuit-dist c-dist
                                      :circuit-fitness c-fit)
                       *population*)
          )))


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


    ; Go ahead and set the first index of *city-array* to be the start city
    (rotatef (aref *city-array* (position *start-city* *city-array* :test (lambda (x y) (eq x (city-num y)))))
             (aref *city-array* 0))
    ))


;;; Select the x fittest individuals out of a list of individuals (those that
;;; have the lowest distance)
;;;
;(defun select-parents (x l) ())


;(defun run () ())


(defun run-tests ()
  (let ((filename "test.txt"))
    (princ (format nil "Test importing from ~s~%" filename))
    (import-data filename)
    (princ (format nil "~%Did it work?~%"))
    (princ (format nil "The file contents are:~%"))
    (with-open-file (stream filename)
      (loop for line = (read-line stream nil 'end)
            until (eq line 'end)
            do (princ (format nil ">>> ~d~%" line))))
    (princ (format nil "~%The stored values are:~%"))
    (princ (format nil "*num-cities* is ~d~%" *num-cities*))
    (princ (format nil "*start-city* is ~d~%" *start-city*))
    (princ (format nil "*city-array* is:~%"))
    (loop for city across *city-array* do
          (princ (format nil
                         " city number: ~d; coordinates: (~d,~d)~%"
                         (city-num city) (city-x city) (city-y city))))
    (princ (format nil "~%Testing the circuit distance function"))
    (princ (format nil "Total distance of that circuit is ~d~%" (calc-dist *city-array*)))
    (princ (format nil "~%Test making an instance of a circuit~%"))
    (defparameter *test-circuit*
      (let ((city-arr
              (randomize-circuit
                (make-array *num-cities*
                            :element-type 'city
                            :initial-contents *city-array*))))
        (defvar circ-dist (calc-dist city-arr))
        (defvar circ-fitness (calc-fitness circ-dist))
        (make-instance 'circuit
                       :cities city-arr
                       :circuit-dist circ-dist
                       :circuit-fitness circ-fitness)
        ))
    (princ (format nil "*test-circuit* is: ~s~%" *test-circuit*))
    (princ (format nil "The city array in that circuit is:~%"))
    (loop for city across (cities *test-circuit*) do
          (princ (format nil
                         " city number: ~d; coordinates: (~d,~d)~%"
                         (city-num city) (city-x city) (city-y city))))
    (princ (format nil "The circuit distance is ~s~%" (circuit-dist *test-circuit*)))
    (princ (format nil "The circuit fitness is ~s~%" (circuit-fitness *test-circuit*)))
    (princ (format nil "Test initializing a population:~%"))
    (initialize-population 10)
    (princ (format nil "An example of a 10-circuit population~%"))
    (loop for circuit across *population*
          for i from 0 below 10 do
          (princ (format nil "index ~d: " i))
          (loop for city across (cities circuit) do
                (princ (format nil "~d " (city-num city))))
          (princ (format nil
                         "d:~7$ f:~7$"
                         (circuit-dist circuit)
                         (circuit-fitness circuit)))
          (princ (format nil "~%")))
    ))


(run-tests)
