;;;;
;;;; Michael Lane
;;;; CS541 - Artificial Intelligence
;;;; Lab 3
;;;; Main Artificial Neural Network
;;;;

(load "nn_utils.fas")

(defvar *initial-training-confusion-matrix* (make-array '(10 10) :initial-element 0))
(defvar *max-training-confusion-matrix* (make-array '(10 10) :initial-element 0))
(defvar *initial-testing-confusion-matrix* (make-array '(10 10) :initial-element 0))
(defvar *max-testing-confusion-matrix* (make-array '(10 10) :initial-element 0))
(defvar *testing-accuracy* 0.0)           ; Current epoch's testing accuracy
(defvar *training-accuracy* 0.0)          ; Current epoch's training accuracy
(defvar *max-training-accuracy* 0.0)      ; Current max training accuracy over all epochs so far
(defvar *max-testing-accuracy* 0.0)       ; Current max testing accuracy over all epochs so far
(defvar *bottom-layer-weight-matrix* nil) ; The stored value of the network for later use
(defvar *top-layer-weight-matrix* nil)    ; The stored value of the network for later use
(defvar *historic-accuracies*)            ; Accuracy of training and testing set for each epoch
()


#|
 | Train the network
 |
 |#
(princ (format nil "Starting Epochs~%"))
(princ (format nil "~%Training progress:~%"))
(princ (format nil "                            25%                      50%                      75%                      100%~%"))
(princ (format nil "     ------------------------|------------------------|------------------------|------------------------|~%"))
(loop for epoch-num from 0 below 2 do
      ;; First shuffle the training data before each epoch
      (setf *training-data* (shuffle *training-data*))

      (princ (format nil "~3,'0d: " epoch-num))

      (loop for i from 0 to 60000
            for input across *training-data* do
            (if (= 0 (mod i (/ 60000 100)))
              (princ (format nil "*")))
            (forward-propagate input)
            (calculate-error-terms input)
            (update-weights input))
      )
(break)

#|
 | Todo list:
 | 1) Test evaluate function
 |X2) Make 4 confusion matrices (training and testing data, initial and max)
 |X3) Make accuracy vars for training and testing data
 | 4) After each epoch evaluate accuracy of training and testing set
 | 5) Append accuracy to a vector #((epoch-num train-accuracy test-accuracy))
 | 6) After epoch 1, update initial and max confusion matrices for training
 |    and testing sets
 | 7) After each epoch, if accuracy is greater than current max, overwrite max
 |    confusion matrix for training and testing sets
 | 8) Add timing outputs for each training epoch, the total training time, the
 |    total time to classify a set.
 | 9) Output confusion matrices and accuracy after each epoch vector to a csv
 |    file. Make sure the data includes the epoch number as well as the accuracy.
 |10) Create graphs either in jupyter or excel using the CSV files.
 |#
