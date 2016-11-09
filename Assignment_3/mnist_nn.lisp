;;;;
;;;; Michael Lane
;;;; CS541 - Artificial Intelligence
;;;; Lab 3
;;;; Math and utility functions for Lab 3
;;;;


#|
 | Takes mnist csv file and makes a list out of it that consists of
 | the target value in the car and a vector of the activations as the
 | cdr.
 |
 | string => ((num #(num)))
 |
 |#
(defun listify (filename)
  (with-open-file (input filename)
    (loop for line = (read-line input nil) while line
          collect (let ((target nil)
                        (data nil))
                    (setf data
                          (read-from-string (substitute #\Space #\, (format nil "(~d)~%" line))))
                    (setf target (pop data))
                    (setf data (append '(1.0) (map 'list #'(lambda (x) (/ x 255.0)) data)))
                    (list target data)))))



#|
 | Return the mean of the values over all columns as a list. Keeping this since
 | I made it work, but this isn't required for the MNIST data set.
 |
 | ((num #(num))) => (num)
 |
 |#
(defun column-means (data)
  (let ((len (float (length (cdar data)))))
    (flet ((col (i) (mapcar #'(lambda (row) (aref row i)) (mapcar #'cdr data))))
      (loop for n from 0 below len
            collect (/ (reduce #'+ (col n)) len)))))


#|
 | Return the standard deviation of the values over all columns as a list. Must pass
 | in the value of the result of column-means. Keeping this since I made it
 | work, but it isn't required for the mnist data set.
 |
 | ((num #(num)) (num)) => (num)
 |
 |#
(defun column-stddev (data col-means)
  (let ((len (float (length (cdar data)))))
    (flet ((col (i) (mapcar #'(lambda (row) (aref row i)) (mapcar #'cdr data))))
      (loop for n from 0 below len
            collect (sqrt (/ (reduce #'+
                                     (mapcar
                                       #'(lambda (x) (expt (- x (elt col-means n)) 2))
                                       (col n)))
                             len))))))


#|
 | Standardize the data. Take in a data set, a list of column-means, and a list of
 | column-stddev and return the data set modified as follows:
 |
 | x_i' = (x_i - mu_i) / stddev_i
 |
 | Keeping this since I made it work, but it isn't required for the MNIST data set.
 |
 |#
(defun standardize (data cm cs)
  (let ((pixel-vals (mapcar #'cdr data)))
    (loop for activations in pixel-vals do
          (loop for x across activations
                for i below (length activations) do
                (cond ((not (eq (elt cs i) 0))
                       (setf (elt activations i) (/ (- x (elt cm i)) (elt cs i)))))))))


#|
 | Vector dot product takes 2 vectors as lists (x1 x2 ... xn), (y1 y2 ... yn)
 | and makes a new list ((* x1 y1) (* x2 y2) ... (* xn yn)) and then returns
 | the resulting sum of that new list. The values of the vectors are coerced
 | to be floats first.
 |
 | I wrote this before I decided I wouldn't be using it.
 |
 | (#(float) #(float)) => float
 |
 |#
(defun dot-product (x y) (reduce #'+ (mapcar #'* (mapcar #'float x) (mapcar #'float y))))


#|
 | transpose an array
 |
 | Another unused function.
 |
 | #(num) => #(num)
 |
 |#
(defun transpose (a)
  (let* ((m (array-dimension a 0))
         (n (array-dimension a 1))
         (b (make-array `(,n ,m) :initial-element 0)))
    ;; Nothing too fancy, just walking over the matrix setting the appropriate values.
    (loop for i from 0 below m do
          (loop for j from 0 below n do
                (setf (aref b j i)
                      (aref a i j))))
    b))


#|
 | Returns an m x n matrix with values x | x \in [-range/2, range/2)
 |
 | m is the number of input activations. Don't forget your bias!
 | n is the number of output activations.
 |
 | If, for example, the matrix is for the MNIST input layer and the hidden
 | layer has 500 activations, and the range is from -0.25 to 0.25, then the
 | call should be:
 |
 | (initialize-weight-vector 785 500 0.5)
 |
 | (int int float) => ((float))
 |
 |#
(defun initialize-weight-matrix (m n range)
	(loop for i from 0 below m
				collect
				(loop for i from 0 below n
							collect
							(- (random range (make-random-state t)) (/ range 2.0)))
				))




#|
 | Multiply two matrices together. Note, this works for vectors, too, but the
 | vectors must be a list of (a) list(s), e.g. '((1 2 3)) or '((1) (2) (3)). The
 | function requires that you give it matrices and vectors that are in the
 | correct orientation. The number of columns of a must equal the number of
 | rows of b.
 |
 | The idea is that a should be a 1 x m array where m is the number of input
 | activations, and b should be an m x n array where n is the number of hidden
 | layer (or output) activations.
 |
 | (#(num) #(num)) => #(num)
 |
 |#
(defun matrix-multiply (a b)
 (mapcar
   (lambda (row)
     (apply #'mapcar
            (lambda (&rest column)
              (apply #'+ (mapcar #'* row column)))
            b))
   a))

(defvar *test-data* (listify "mnist_test_10.csv"))
(defvar *test-weight-matrix* (initialize-weight-matrix 785 5 0.5))
(defvar *hidden-activations* (append '(1.0)
                                     (car (matrix-multiply
                                            (cdr (elt *test-data* 0))
                                            *test-weight-matrix*))))
(print *hidden-activations*)


#|
 | Returns 1 / (1 - e ^ -x). The x value is coerced to be a float first.
 |
 | num => float
 |
 |#
(defun sigmoid (x) (/ 1 (+ 1 (exp (- (float x))))))



(defun forward-propagate)


#|
 |(defun run-tests ()
 |  ;; Test making a 3x5 array filled with random values between -0.25 and 0.25
 |  (print (initialize-weight-matrix 3 5 0.5))
 |  (print (matrix-multiply
 |    (make-array '(1 5) :initial-contents '((1 2 3 4 5)))
 |    (make-array '(5 3) :initial-contents '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15)))))
 |
 |  ;; Test getting input and creating a larger weight matrix
 |  (let ((*test-data* (listify "mnist_test_10.csv"))
 |        (*test-weight-matrix* (initialize-weight-matrix 785 5 0.5)))
 |    (loop for item in *test-data* do
 |          (print item))
 |
 |    (aref *test-weight-matrix* 0 0)
 |
 |    ;; Test feeding the activations forward to an output of 5 nodes
 |    (print (matrix-multiply (cdr (elt *test-data* 0))
 |                            *test-weight-matrix*))
 |    ))
 |
 |
 |(run-tests)
 |#

