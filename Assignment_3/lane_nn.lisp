;;;;
;;;; Michael Lane
;;;; CS541 - Artificial Intelligence
;;;; Lab 3
;;;; Main Artificial Neural Network
;;;;

(load "nn_utils.lisp")


#|
 | Set up the training and testing data
 |
 |#
(defvar *training-data* (elt training-info 2))
(shuffle *training-data*)
(defvar *testing-data* (elt testing-info 2))


#|
 | Set up the other data structures we need.
 |
 |#
(defvar *hidden-activations* (append '(1.0) (make-list *number-hidden-activations*)))
(defvar *hidden-error-terms* (make-list *number-hidden-activations*))
(defvar *output-activations* (make-list *number-output-activations*))
(defvar *output-error-terms* (make-list *number-output-activations*))


#|
 | Randomize a matrix of randomized weights for input to hidden and hidden to
 | output
 |
 |#
(defvar *input-to-hidden-weight-matrix*
  (initialize-weight-matrix *number-input-activations*
                            *number-hidden-activations*
                            0.5))
(defvar *hidden-to-output-weight-matrix*
  (initialize-weight-matrix (1+ *number-hidden-activations*)
                            *number-output-activations*
                            0.5))


#|
 | Initialize a data structure to hold previous weight changes to facilitate
 | using the momentum to prevent oscillations
 |
 |#
(defvar *previous-output-weight-change-matrix*
  (make-list (1+ *number-hidden-activations*)
             :initial-element (make-list *number-output-activations*
                                         :initial-element 0)))
(defvar *previous-hidden-weight-change-matrix*
  (make-list (1+ *number-input-activations*)
             :initial-element (make-list *number-hidden-activations*
                                         :initial-element 0)))


#|
 | Train the network
 |
 |#
(loop for epoch-num from 0 below *number-of-epochs* do
      (princ (format nil "Starting Epoch ~d~%" epoch-num))
      ;; First shuffle the training data before each epoch
      (setf *training-data* (shuffle *training-data*))
      (loop for input in *training-data* do
            (print input)))
