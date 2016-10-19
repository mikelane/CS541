;;;;
;;;; Michael Lane
;;;; 11/01/2016
;;;; CS541
;;;; Lab 2
;;;;
;;;; Implements a Genetic Algorithm to determine an acceptible solution to the
;;;; traveling salesman problem.
;;;;


;;; Import the data from the file into an alist of (city (x, y)) pairs.
;;;
(defun import-data (f) ())


;;; Calculate the euclidean distance from one city to the next
;;;
(defun dist (a b) ())


;;; Initialize a set of lists of n individuals drawn from an alist of city, location pairs
;;;
(defun initialize-population (n l) ())


;;; Given a list of cities, (x_1, x_2, ..., x_n), return the total hamiltonian circuit
;;; distance from x_1 -> x_2 -> ... -> x_n -> x_1.
;;;
(defun fitness (l) ())


;;; Select the x fittest individuals out of a list of individuals (those that have the lowest distance)
;;;
(defun select-parents (x l) ())


;;; Swap two random values in a list
;;;
(defun random-swap (a) ())


;;; Mate the list of the surviving parents together using a roulette wheel selection
;;; mechanism and perform a swap mutation on x% of the population
;;;
(defun crossover-mutation (x l) ())


;;; Select parents to mate using roulette wheel selection and generate children until the
;;; population is full again.
;;;
(defun replace-population (l) ())


(defun run () ())
