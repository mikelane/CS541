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






;;; The circuit class. A circuit is a collection of cities, the total distance of
;;; the Hamiltonian path, and the fitness of that class
;;;
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
     :accessor circuit-fitness)
   (circuit-weight
     :initform 0
     :accessor circuit-weight)))


;;; Wrapper function for city-dist. We need this wrapper since we have to add in
;;; the value of the distance from the last city to the start city
;;;
(defmethod calc-dist ((a circuit))
  ;; If there is 0 or 2 cities, this should return nil
  (cond ((< (length (cities a)) 2) nil)
        ((eq (length (cities a)) 2) (* 2 (city-dist (aref (cities a) 0)
                                                    (aref (cities a) 1))))
        (t (+ (city-distance (aref (cities a) 0)
                             (aref (cities a) (1- (length (cities a)))))
              (calc-dist-rec (cities a))))
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


;;; Set the circuit class's fitness.
;;;
(defmethod calc-fitness ((a circuit))
  (cond ((eq 0 (circuit-dist a)) nil)
        (t (/ 1 (circuit-dist a)))))


;;; Make it so the circuit class initializes its distance and fitness
;;; automatically when it is initialized
;;;
(defmethod initialize-instance :after ((a circuit) &key)
  (setf (circuit-dist a) (calc-dist a))
  (setf (circuit-fitness a) (calc-fitness a)))


;;; Circuit class method for random mutation. Pick two random values and swap
;;; the values at the index in the cities array
;;;
(defmethod circuit-mutate ((a circuit))
  (rotatef (aref (cities a) (1+ (random (1- *num-cities*) (make-random-state t))))
           (aref (cities a) (1+ (random (1- *num-cities*) (make-random-state t)))))
  ;; We need to keep the distance and fitness up-to-date
  (setf (circuit-dist a) (calc-dist a))
  (setf (circuit-fitness a) (calc-fitness a)))


;;; Print the information about a circuit:
(defmethod circuit-print ((a circuit))
  (loop for city across (cities a)
        for i from 1 to *num-cities* do
        (princ (format nil
                       " ~d: city: ~d location: (~d,~d)~%"
                       i (city-num city) (city-x city) (city-y city))))
  (princ (format nil
                 " ~d: city: ~d location: (~d,~d)~%"
                 (1+ *num-cities*)
                 (city-num (aref (cities a) 0))
                 (city-x (aref (cities a) 0))
                 (city-y (aref (cities a) 0))))
  (princ (format nil "The circuit distance is ~s~%" (calc-dist a)))
  (princ (format nil "The circuit fitness is ~s~%" (calc-fitness a))))





;;; Genetic algorithm Hyperparameters
;;;
(defparameter *population-number* 100)
(defparameter *percentage-elite* 20)
(defparameter *percent-to-mutate* 5)
(defparameter *number-of-generations* 100)
(defparameter *max-unchanged-generations-allowed* 100)



;;; Define the necessary variables
;;;
(defvar *num-cities* 0)
(defvar *city-array* nil)
(defvar *start-city* nil)
(defvar *population*
  (make-array *population-number*
              :element-type 'circuit
              :fill-pointer 0
              :initial-element nil))
(defvar *fittest-individual* nil)
(defvar *unchanged-fittest-count* *max-unchanged-generations-allowed*)
(defvar *output-filename*
  (pathname
    (format nil
            "output/TSP_GA_OUT_~d.data"
            (get-universal-time))))







;;; Circuit class method for crossover. Take two circuits, find a subsequence
;;; from the first and place it in the child in the same location. Then for each
;;; city from the 2nd that is not in the subsequence from the first, add that to
;;; the child in the order it is found in the 2nd parent.
;;;
(defmethod circuit-crossover-mutate ((a circuit) (b circuit))
  ;; Choose an i such that [0, *num-cities*)
  (let ((i (random (1- *num-cities*) (make-random-state t)))
        (j 0)
        (y 0)
        (result (make-array *num-cities*
                            :element-type 'city
                            :initial-element nil)))

    ;; Generate a random value for j between 1 and the length of the list + 1
    (setf j (1+ (+ (1+ i) (random (- *num-cities* (1+ i)) (make-random-state t)))))

    ;; Assign the cities[i,j] to result[i,j]
    (setf (subseq result i j) (subseq (cities a) i j))

    ;; Set the b index to the position of the first nil
    (setf y (position 'nil result))

    ;; Loop over the cities of b
    (loop for city across (cities b) do
          ;; If the city under consideration is not already in the result,
          (if (not (position city result))
            ;; then set result at index y to the city
            (progn (setf (aref result y) city)
                   ;; update the y index to the next nil value or nil if none
                   ;; are found
                   (setf y (position 'nil result)))))

    ;; Change the result into a circuit object
    (setf result (make-instance 'circuit :cities result))

    ;; Generate a random number and if it is less than the percentage to
    ;; mutate hyperparameter, then mutate the result
    (if (< (1+ (random 101 (make-random-state t)))
           *percent-to-mutate*)
      (circuit-mutate result))

    ;; Now push that result onto the population
    (vector-push result *population*)))




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




;;; Initialize a vector of n randomized circuits. This is the initial population
;;; for the Genetic Algorithm.
;;;
(defun initialize-population (n)
  (loop for i from 0 below n do
        ;; Push the circuit instance onto the population
        (vector-push
          ;; Make an instance of a circuit with the randomized city circuit
          (make-instance 'circuit
                         :cities
                         ;; Make a randomized city circuit
                         (randomize-circuit
                           (make-array *num-cities*
                                       :element-type 'city
                                       :initial-contents *city-array*)))
          *population*)))




;;; Print a human-readable version of the current population. Useful for
;;; testing purposes.
;;;
(defun print-population ()
  (loop for circuit across *population*
        for i from 0 below *population-number* do
        (princ (format nil "index ~d:" i))
        (loop for city across (cities circuit) do
              (princ (format nil "~d " (city-num city))))
        (princ (format nil
                       "  d:~7$ f:~7$"
                       (circuit-dist circuit)
                       (circuit-fitness circuit)))
        (princ (format nil "~%"))))




;;; Select the elite population. Sort the population by fitness in descending
;;; order then use the *percentage-elite* hyperparameter to determine how many
;;; elite individuals to keep. Discard unkept individuals.
;;;
;;; Return the current location of the fill pointer
;;;
(defun select-parents ()
  (let ((n (floor (* (/ *percentage-elite* 100) *population-number*))))
    (sort *population* #'> :key #'circuit-fitness)
    (loop for i from 0 below (- (length *population*) n) do
          (vector-pop *population*))
    ;; After selecting parents, update the *fittest-individual* if required
    (if (or
          (eq nil *fittest-individual*)
          (< (circuit-fitness *fittest-individual*)
             (circuit-fitness (aref *population* 0))))
      (progn (setf *fittest-individual*
                   (aref *population* 0))
             ;; Output the details of the new fittest individual
             (princ (format nil
                            "Current Fittest Individual:~%    tour:~s~%    dist:~5$ fitness:~9$~%"
                            (append (loop for city across (cities *fittest-individual*)
                                          collect (city-num city))
                                    `(,(city-num (aref (cities *fittest-individual*) 0))))
                            (circuit-dist *fittest-individual*)
                            (circuit-fitness *fittest-individual*)))
             (setf *unchanged-fittest-count* *max-unchanged-generations-allowed*))
      ;; else, decrement the *unchanged-fittest-count* counter
      (decf *unchanged-fittest-count*))

    ;;; Write the data to the file for fun graphing later
    (with-open-file (stream
                      (ensure-directories-exist
                        *output-filename*)
                      :direction :output
                      :if-does-not-exist :create
                      :if-exists :append)
      (format stream
              "~s ~7$ ~7$~%"
              (append (loop for city across (cities *fittest-individual*)
                            collect (city-num city))
                      `(,(city-num (aref (cities *fittest-individual*) 0))))
              (circuit-dist *fittest-individual*)
              (circuit-fitness *fittest-individual*)))

    ;; Return the current location of the fill pointer
    (fill-pointer *population*)))




;;; Replace the population. This method follows the algorithm of MMEVCA:
;;; 1) The fitness of the *population-number* individuals is computed
;;; 2) The population is sorted in descending order of fitness
;;; 3) The *percent-to-mutate* variable is used to determine the percent of the
;;;    highest fitness ("elite") individuals which are copied to the next
;;;    generation unmodified.
;;; 4) The remaining individuals are formed by single-point crossovers between
;;;    randomly chosen (with replacement) pairs of elite individuals. In
;;;    addition, *percent-to-mutate*
;;;
(defun replace-population ()
  ;; Get the location of the current fill pointer (which also selects the elites)
  (let ((current (select-parents)))
    ;; Fill the rest of the *population* vector with new individuals
    (loop for i from current below *population-number* do
            (circuit-crossover-mutate
              (aref *population* (random current (make-random-state t)))
              (aref *population* (random current (make-random-state t)))))
    ))




;;; Evolve the population. Keep evolving the population until the max number of
;;; generations has been reached or the max number of allowed generations
;;; without a change in the fittest individual occurs.
;;;
(defun evolve ()
  (loop for i from 0 below *number-of-generations*
        while (> *unchanged-fittest-count* 0) do
        (replace-population)))




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




;;; Run the program with all the inputs.
;;;
(defun run (filename)
  (princ (format nil
                 "
Hyperparameters:
    *population-number*: ~d
    *percentage-elite*: ~d
    *percent-to-mutate*: ~d
    *number-of-generations*: ~d
    *max-unchanged-generations-allowed*: ~d

"
                 *population-number*
                 *percentage-elite*
                 *percent-to-mutate*
                 *number-of-generations*
                 *max-unchanged-generations-allowed*))
  (princ (format nil "Importing data ..."))
  (import-data filename)
  (princ (format nil " DONE"))

  (princ (format nil "~%Initializing Population ..."))
  (initialize-population *population-number*)
  (princ (format nil " DONE"))

  (princ (format nil "~%Evolving population ..."))
  (evolve)
  (princ (format nil "DONE"))

  (princ (format nil "~%~%Shortest Circuit:~%"))
  (circuit-print *fittest-individual*)
  )




#|
 |;;; Run tests of the various functionality
 |;;;
 |(defun run-tests ()
 |  (let ((filename "test.txt"))
 |    (princ (format nil "Test importing from ~s~%" filename))
 |    (import-data filename)
 |    (princ (format nil "~%Did it work?~%"))
 |    (princ (format nil "The file contents are:~%"))
 |    (with-open-file (stream filename)
 |      (loop for line = (read-line stream nil 'end)
 |            until (eq line 'end)
 |            do (princ (format nil ">>> ~d~%" line))))
 |    (princ (format nil "~%The stored values are:~%"))
 |    (princ (format nil "*num-cities* is ~d~%" *num-cities*))
 |    (princ (format nil "*start-city* is ~d~%" *start-city*))
 |    (princ (format nil "*city-array* is:~%"))
 |    (loop for city across *city-array* do
 |          (princ (format nil
 |                         " city number: ~d; coordinates: (~d,~d)~%"
 |                         (city-num city) (city-x city) (city-y city))))
 |
 |    ;;;;;;;;;;;;;;;;
 |
 |    (princ (format nil "~%Test making an instance of a circuit~%"))
 |    (defvar *test-circuit*
 |      (let ((city-arr
 |              (randomize-circuit
 |                (make-array *num-cities*
 |                            :element-type 'city
 |                            :initial-contents *city-array*))))
 |        (make-instance 'circuit :cities city-arr)
 |        ))
 |    (princ (format nil "*test-circuit* is: ~s~%" *test-circuit*))
 |    (princ (format nil "~%Testing the circuit distance function"))
 |    (princ (format nil "The city array in that circuit is:~%"))
 |    (circuit-print *test-circuit*)
 |
 |    ;;;;;;;;;;;;;;;;
 |
 |    (princ (format nil "~%Test initializing a population:~%"))
 |    (initialize-population *population-number*)
 |    (print-population)
 |
 |    ;; Removed in lieu of testing replace-population()
 |    ;(princ (format nil "~%Test random mutation for all circuits in population~%"))
 |    ;(loop for circuit across *population* do
 |          ;(circuit-mutate circuit))
 |    ;(princ (format nil "The new values of the population:~%"))
 |    ;(print-population)
 |
 |    ;(princ (format nil "~%Test selecting the population without removing any~%"))
 |    ;(select-parents)
 |    ;(print-population)
 |
 |    ;(princ (format nil "~%Test selecting the population without removing all but 2~%"))
 |    ;(select-parents)
 |    ;(print-population)
 |
 |    ;(princ (format nil "~%Test crossover/mutation~%"))
 |    ;(circuit-crossover-mutate (aref *population* 0) (aref *population* 1))
 |    ;(print-population)
 |
 |    ;; Removed in favor of testing the run function
 |    ;(princ (format nil "~%Test replace_population:~%"))
 |    ;(replace-population)
 |    ;(princ (format nil "Population after calling replace-population:~%"))
 |    ;(print-population)
 |
 |    ;(princ (format nil "~%Test replace-population 100 times:~%"))
 |    ;(loop for i from 0 below 100 do
 |          ;(replace-population))
 |    ;(princ (format nil "Population after calling replace-population 100 times:~%"))
 |    ;(print-population)
 |
 |    ;(princ (format nil "~%Test printing the information about the fittest individual:~%"))
 |    ;(circuit-print *fittest-individual*)
 |
 |    ))
 |#





;;; Output a helpful CLI message
(defun print-help-message ()
  (princ
"
usage: clisp TSP_EA.lisp filename [option value]

REQUIRED
  filename: filename of the data file for the TSP

OPTIONAL: You can include a series of option value pairs after the filename in
          order to set the values of the hyperparameters. The option and values
          are described below.


  The available hyperparameters are:

    hyperparameter                      |  value  |  Command line option
  ------------------------------------------------------------------------
    *population-number*                 |  100    |  popnum
    *percentage-elite*                  |  20     |  eliteperc
    *percent-to-mutate*                 |  5      |  mutateperc
    *number-of-generations*             |  100    |  gennum
    *max-unchanged-generations-allowed* |  100    |  maxunch


 example: clisp TSP_EA.lisp test.txt popnum 100 mutateperc 10 maxunch 100

"))




;;; Parse the command line args and run the algorithm
;;;
(defun parse-command-line-and-run ()
  ;; Pop the filename from the args list
  (let ((filename (pop ext:*args*))
        (option nil)
        (value nil))
    ;; If there wasn't a filename, exit
    (if (eq filename nil)
      (progn (print-help-message)
             (ext:exit 1))
      ;; otherwise, make the filename a pathname
      (progn (princ (format nil "Running Genetic Algorithm. Data file: ~s~%" filename))
             (setf filename (pathname filename))))

    (loop while ext:*args* do
          (setf option (pop ext:*args*))
          (setf value (handler-case
                        (parse-integer (pop ext:*args*))
                        (error (c)
                               (princ (format nil "~s~%" c)))))
          (cond ((and value (string= option "popnum"))
                 (setf *population-number* value))
                ((and value (string= option "eliteperc"))
                 (setf *percentage-elite* value))
                ((and value (string= option "mutateperc"))
                 (setf *percent-to-mutate* value))
                ((and value (string= option "gennum"))
                 (setf *number-of-generations* value))
                ((and value (string= option "maxunch"))
                 (setf *max-unchanged-generations-allowed* value))
                ))

    (run filename)))

(parse-command-line-and-run)

