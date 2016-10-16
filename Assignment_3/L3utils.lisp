;;;;
;;;; Michael Lane
;;;; CS541 - Artificial Intelligence
;;;; Lab 3
;;;; Math and utility functions for Lab 3
;;;;


;;; Takes mnist csv file and makes a list out of it that consists of
;;; the target value in the car and a vector of the activations as the
;;; cdr.
;;;
;;; string => ((num #(num)))
;;;
(defun listify (filename)
  (with-open-file (input filename)
    (loop :for line := (read-line input nil) :while line
          ;; While there was a line of input, collect the resulting list
          :collect (let ((target nil)  ; local vars
                         (data nil))
                     ;; Remove the commas, parse the values, and store as a list
                     (setf data (read-from-string (substitute #\Space #\, (format nil "(~d)~%" line))))
                     ;; Pop the target value off the list and store it
                     (setf target (pop data))
                     ;; Make the data values a vector, append them to a list with target as the CAR
                     (setf data (append `(,target) (eval (append '(vector) data)))))
          )))


;;; Vector dot product takes 2 vectors as lists (x1 x2 ... xn), (y1 y2 ... yn)
;;; and makes a new list ((* x1 y1) (* x2 y2) ... (* xn yn)) and then returns
;;; the resulting sum of that new list. The values of the vectors are coerced
;;; to be floats first.
;;;
;;; (#(float) #(float)) => float
;;;
(defun dot-product (x y) (reduce #'+ (mapcar #'* (mapcar #'float x) (mapcar #'float y))))


;;; Transpose an array
;;;
;;; #(num) => #(num)
;;;
(defun transpose (a)
	(let* ((m (array-dimension a 0))
				 (n (array-dimension a 1))
				 (b (make-array `(,n ,m) :initial-element 0)))
		(loop for i from 0 below m do
					(loop for j from 0 below n do
								(setf (aref b j i)
											(aref a i j))))
		b))

;;; Multiply two matrices together. Note, this works for vectors, too, but the
;;; vectors must be a list of (a) list(s), e.g. '((1 2 3)) or '((1) (2) (3)). The
;;; function requires that you give it matrices and vectors that are in the
;;; correct orientation. The number of columns of a must equal the number of
;;; rows of b.
;;;
;;; (#(num) #(num)) => #(num)
;;;
(defun matrix-multiply (a b)
  (let* ((m (array-dimension a 0))
         (n (array-dimension a 1))
         (l (array-dimension b 1))
         (result (make-array `(,m ,l) :initial-element 0)))
    (loop for i from 0 below m do
              (loop for k from 0 below l do
                    (setf (aref result i k)
                          (loop for j from 0 below n
                                sum (* (aref a i j)
                                       (aref b j k))))))
    result))


;;; Returns 1 / (1 - e ^ -x). The x value is coerced to be a float first.
;;;
;;; num => float
;;;
(defun sigmoid (x) (/ 1 (+ 1 (exp (- (float x))))))


;;; Returns an m x n matrix with values x | x \in [-range/2, range/2)
;;;
;;; (int int float) => ((float))
;;;
(defun initialize-weight-vector (m n range)
	(make-array `(,m ,n) :initial-contents
							(loop for i from 1 to m
										collect (mapcar (lambda (x) (- (random x) (/ range 2.0)))
																		(make-list n :initial-element (float range))))))


;;; Standardizes the data to prevent an imbalance between features of different
;;; scales. Returns the column value




;;; This should become the basis for the feed forward algorithm.
#|
 |(print
 |  (transpose
 |    (matrix-multiply
 |      (initialize-weight-vector 10 3 1)
 |      (transpose
 |        (mapcar #'(lambda (x) (mapcar #'sigmoid x))
 |                (initialize-weight-vector 1 10 100))
 |        ))
 |    ))
 |#

(defun running-stddev ()
  (let ((sum 0) (sq 0) (n 0))
    (lambda (x)
      (incf sum x) (incf sq (* x x)) (incf n)
      (/ (sqrt (- (* n sq) (* sum sum))) n))))

