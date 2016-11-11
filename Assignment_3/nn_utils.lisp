;;;;
;;;; Michael Lane
;;;; CS541 - Artificial Intelligence
;;;; Lab 3
;;;; Math and utility functions for Lab 3
;;;;

(load "mnist-loader")

(defvar *number-hidden-layers* 1)
;; I'm wondering what a 10 x 10 image of the hidden layer will look like
(defvar *number-hidden-activations* 100)
(defvar *number-input-activations* 785)   ; 784 activations + bias
(defvar *number-output-activations* 10)   ; for digits 0 to 9
(defvar *learning-rate* 0.3)              ; trying what we used in CS545
(defvar *momentum* 0.3)                   ; trying what we used in CS545


#|
 |(defvar *test-data* '((5 (0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9))
 |                      (6 (0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9))))
 |#

#|
 |(setf *number-hidden-activations* 2)
 |(setf *number-input-activations* 9)
 |#

(defvar *hidden-activations* (append '(1.0) (make-list *number-hidden-activations*)))
(defvar *hidden-error-terms* (make-list *number-hidden-activations*))

(defvar *output-activations* (make-list *number-output-activations*))
(defvar *output-error-terms* (make-list *number-output-activations*))


#|
 | Do a Knuth shuffle of the imported-data array. It is important to shuffle the
 | data between each epoch to avoid overfitting.
 |
 | #(a) => #(a)
 |
 |#
(defun shuffle (arr)
  (loop for i from (length arr) downto 2 do
        (rotatef (aref arr (1- i))
                 (aref arr (random i (make-random-state t)))))
  arr)


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
(defun transpose (m)
    (apply #'mapcar #'list m))


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


(defvar *input-to-hidden-weight-matrix*
  (initialize-weight-matrix *number-input-activations*
                            *number-hidden-activations*
                            0.1))


(defvar *hidden-to-output-weight-matrix*
  (initialize-weight-matrix (1+ *number-hidden-activations*)
                            *number-output-activations*
                            0.1))

(defvar *previous-output-weight-change-matrix*
  (make-list (1+ *number-hidden-activations*)
             :initial-element (make-list *number-output-activations*
                                         :initial-element 0)))

(defvar *previous-hidden-weight-change-matrix*
  (make-list (1+ *number-input-activations*)
             :initial-element (make-list *number-hidden-activations*
                                         :initial-element 0)))


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


#|
 | Returns 1 / (1 - e ^ -x). The x value is coerced to be a float first.
 |
 | num => float
 |
 |#
(defun sigmoid (x) (/ 1 (+ 1 (exp (- x)))))


#|
 | Takes in a list and returns a list of the form ((i v1) (j v2)) where i is the
 | index of the minimum value v1 in the list and j is the index of the maximum
 | value v2 in the list
 |
 |#
(defun find-min-max (l)
  (let ((smallest most-positive-double-float)
        (biggest most-negative-double-float)
        (i 0)
        (j 0))
    (loop for v in l
          for x from 0 below (length l) do
          (if (> v biggest)
            (progn (setf biggest v)
                   (setf j x)))
          (if (< v smallest)
            (progn (setf smallest v)
                   (setf i x))))
    `((,i ,smallest) (,j ,biggest))))


#|
 | Forward propagate the activations (plus bias) to the hidden layer (not
 | including the bias), take the sigmoid of the values, and add in a bias. Then
 | forward propagate the hidden activations and the bias to the output activations
 | layer and then take the sigmoid of the results. Finally, calculate the error
 | terms.
 |
 |#
(defun forward-propagate (input)
  ;; Forward propagate to the hidden layer
  (setf (cdr *hidden-activations*)
        (map 'list #'sigmoid
             (car (matrix-multiply
                    (cdr input)
                    *input-to-hidden-weight-matrix*))))

  ;; Forward propagate to the output layer
  (setf *output-activations*
        (map 'list #'sigmoid
             (car (matrix-multiply
                    (list *hidden-activations*)
                    *hidden-to-output-weight-matrix*))))

  ;; Return the min and max indices and values
  (find-min-max *output-activations*))


#|
 | Calculate the error terms of the output and hidden nodes. The output error is
 |
 | error_k = o_k * (1 - o_k) * (t_k - o_k)
 |
 | Where error_k is the error term of the kth output node, o_k is the value of
 | the kth output after forward prop, and t_k is the target value of the kth
 | output node. The hidden layer is:
 |
 | error_j = h_j * (1 - h_j) * (Sum_{k \in output units}(weight_{kj} * error_k))
 |
 | Where error_j is the error term of thekth hidden node, weight_{kj} is the
 | weight of the edge from the jth hidden node to the kth output node and
 | error_k is the output error at the kth node.
 |
 |#
(defun calculate-error-terms (input)
  ;; Calculate error terms of output nodes.
  (setf *output-error-terms*
        (loop for unit in *output-activations*
              for i from 0 below *number-output-activations*
              collect (if (eq i (car input))
                        (* unit (- 1 unit) (- 1 unit))
                        (* unit (- 1 unit) (- 0 unit)))))

  ;; Calculate error terms of hidden nodes
  (setf *hidden-error-terms*
        (loop for hidden-unit in (cdr *hidden-activations*)
              for weight-vector in (cdr *hidden-to-output-weight-matrix*)
              collect (* hidden-unit
                        (- 1 hidden-unit)
                        (caar (matrix-multiply
                                (list *output-error-terms*)
                                (transpose (list weight-vector))))))))

#|
 |(print (forward-propagate (car *test-data*)))
 |(calculate-error-terms (car *test-data*))
 |#



#|
 | Update the weight at the current step. Update each weight at the current step,
 | w_kj_t from the hidden to output layer:
 |
 | w_kj_t <- w_kj + (learning rate * error_k * h_j + momentum * w_kj_t-1)
 |
 | Update each weight at the current step w_ji_t from the input to the hidden
 | layer:
 |
 | w_ji <- w_ji + (learning rate * error_j * input activation i + momentum w_kj_t-1)
 |
 |#
(defun update-weights (input)
  ;; Error from hidden to output
  (let ((cur-Del-w-vect nil))
    (loop for weight-vector in *hidden-to-output-weight-matrix*
          for j from 0 below (length *hidden-to-output-weight-matrix*)
          for prev-weight-change-vector in *previous-output-weight-change-matrix*
          for h in *hidden-activations* do
          ;(princ (format nil "~%----------------------------------------------~%"))
          ;(princ (format nil "original weight vector: ~s~%" weight-vector))
          ;(princ (format nil "----------------------------------------------~%"))
          ;(princ (format nil "    h: ~s~%" h))
          ;(princ (format nil "    output activations: ~s~%" *output-activations*))
          ;(princ (format nil "    output error terms: ~s~%" *output-error-terms*))
          ;(princ (format nil "    prev-weight-change-vector: ~s~%" prev-weight-change-vector))
          ;(princ (format nil "~%----------------------------------------------~%"))
          ;(princ (format nil "weight matrix: ~s~%" *hidden-to-output-weight-matrix*))
          ;(princ (format nil "----------------------------------------------~%"))
          (setf cur-Del-w-vect
                (loop for del-k in *output-error-terms*
                      for Del-w in prev-weight-change-vector
                      collect (+ (* *learning-rate* del-k h)
                                 (* *momentum* Del-w))))
          (setf (elt *previous-output-weight-change-matrix* j)
                (setf prev-weight-change-vector cur-Del-w-vect))
          (setf (elt *hidden-to-output-weight-matrix* j)
                (setf weight-vector (mapcar #'+ weight-vector cur-Del-w-vect)))
          ;(princ (format nil "~%----------------------------------------------~%"))
          ;(princ (format nil "*previous-output-weight-change-matrix*: ~s~%"
                         ;*previous-output-weight-change-matrix*))
          ;(princ (format nil "updated weight vector: ~s~%" weight-vector))
          ;(princ (format nil "----------------------------------------------~%"))
          )
    (loop for weight-vector in *input-to-hidden-weight-matrix*
          for i from 0 below (length *input-to-hidden-weight-matrix*)
          for prev-weight-change-vector in *previous-hidden-weight-change-matrix*
          for x in (cadr input) do
          ;(princ (format nil "~%----------------------------------------------~%"))
          ;(princ (format nil "original weight vector: ~s~%" weight-vector))
          ;(princ (format nil "----------------------------------------------~%"))
          ;(princ (format nil "    x: ~s~%" x))
          ;(princ (format nil "    hidden activations: ~s~%" (cdr *hidden-activations*)))
          ;(princ (format nil "    hidden error terms: ~s~%" *hidden-error-terms*))
          ;(princ (format nil "    prev-weight-change-vector: ~s~%" prev-weight-change-vector))
          ;(princ (format nil "~%----------------------------------------------~%"))
          ;(princ (format nil "weight matrix: ~s~%" *input-to-hidden-weight-matrix*))
          ;(princ (format nil "----------------------------------------------~%"))
          (setf cur-Del-w-vect
                (loop for del-j in *hidden-error-terms*
                      for Del-w in prev-weight-change-vector
                      collect (+ (* *learning-rate* del-j x)
                                 (* *momentum* Del-w))))
          (setf (elt *previous-hidden-weight-change-matrix* i)
                (setf prev-weight-change-vector cur-Del-w-vect))
          (setf (elt *input-to-hidden-weight-matrix* i)
                (setf weight-vector (mapcar #'+ weight-vector cur-Del-w-vect)))
          )))

#|
 |(print "=====")
 |(print "original *input-to-hidden-weight-matrix*")
 |(print *input-to-hidden-weight-matrix*)
 |(print "original *hidden-to-output-weight-matrix*")
 |(print *hidden-to-output-weight-matrix*)
 |(print "=====")
 |(update-weights (car *test-data*))
 |
 |(print "=====")
 |(print "*output-activations*")
 |(print *output-activations*)
 |(print "updated *input-to-hidden-weight-matrix*")
 |(print *input-to-hidden-weight-matrix*)
 |(print "updated *hidden-to-output-weight-matrix*")
 |(print *hidden-to-output-weight-matrix*)
 |
 |(print (forward-propagate (car *test-data*)))
 |(calculate-error-terms (car *test-data*))
 |(update-weights (car *test-data*))
 |(print "=====")
 |(print "*output-activations*")
 |(print *output-activations*)
 |(print "updated *input-to-hidden-weight-matrix*")
 |(print *input-to-hidden-weight-matrix*)
 |(print "updated *hidden-to-output-weight-matrix*")
 |(print *hidden-to-output-weight-matrix*)
 |
 |#

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


