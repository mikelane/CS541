;;;;
;;;; Michael Lane
;;;; 11/15/16
;;;;
;;;; CS541 - Artificial Intelligence
;;;; Lab 3
;;;; Main Artificial Neural Network
;;;;

(load "nn_utils.fas")

(defvar *training-confusion-matrix* (make-array '(10 10) :initial-element 0))
(defvar *max-training-confusion-matrix* (make-array '(10 10) :initial-element 0))
(defvar *testing-confusion-matrix* (make-array '(10 10) :initial-element 0))
(defvar *max-testing-confusion-matrix* (make-array '(10 10) :initial-element 0))

(defvar *testing-accuracy* 0.0)                 ; Current epoch's testing accuracy
(defvar *training-accuracy* 0.0)                ; Current epoch's training accuracy
(defvar *max-training-accuracy* 0.0)            ; Current max training accuracy over all epochs so far
(defvar *max-testing-accuracy* 0.0)             ; Current max testing accuracy over all epochs so far
(defvar *bottom-layer-weight-matrix* nil)       ; The stored value of the network for later use
(defvar *top-layer-weight-matrix* nil)          ; The stored value of the network for later use
(defvar *historic-accuracies*
  (make-array 1 :adjustable t :fill-pointer 0)) ; Accuracy of training and testing set for each epoch

(defparameter training-cm-filename
  (format nil
          "train-max-conf-matrix-~d-~d-~d-~d.csv"
          *learning-rate*
          *momentum*
          *number-of-epochs*
          *number-hidden-activations*))

(defparameter testing-cm-filename
  (format nil
          "test-max-conf-matrix-~d-~d-~d-~d.csv"
          *learning-rate*
          *momentum*
          *number-of-epochs*
          *number-hidden-activations*))

(defparameter accuracies-filename
  (format nil
          "accuracies~d-~d-~d-~d.csv"
          *learning-rate*
          *momentum*
          *number-of-epochs*
          *number-hidden-activations*))

#|
 | Train and evaluate the network
 |
 |#
(defun train-and-evaluate ()
  (princ (format nil "Starting Epochs~%"))
  (princ (format nil "~%Training progress:~%"))
  (princ (format nil "                            25%                      50%                      75%                      100%~%"))
  (princ (format nil "     ------------------------|------------------------|------------------------|------------------------|~%"))
  (loop for epoch-num from 1 to *number-of-epochs*
        while (< *training-accuracy* 1) do

        ;; First shuffle the training data before each epoch
        (setf *training-data* (shuffle *training-data*))

        (princ (format nil "~3,'0d: " epoch-num))

        ;; Do one epoch's worth of training
        (loop for i from 0 below *number-training-images*
              for input across *training-data* do
              (if (= 0 (mod i (/ 60000 100)))
                (princ (format nil "*")))
              (forward-propagate input)
              (calculate-error-terms input)
              (update-weights input))

        ;; Evaluate the accuracy of the training data
        (princ (format nil " Evaluating accuracy ... "))
        (let ((guess nil)
              (num-correct 0))
          (loop for input across *training-data* do
                (setf guess (classify input))
                (if (= guess (car input))
                  (incf num-correct))
                (incf (aref *training-confusion-matrix* (car input) guess)))
          ;; Calculate the accuracy = num-correct / number of images
          (setf *training-accuracy* (float (/ num-correct *number-training-images*)))
          ;; Keep track of the max training accuracy and its confusion matrix
          (if (> *training-accuracy* *max-training-accuracy*)
            (progn (setf *max-training-accuracy* *training-accuracy*)
                   (setf *max-training-confusion-matrix* (copy-array *training-confusion-matrix*))))
          ;; Keep the 1st epoch's confusion matrix for funsies
          (if (= epoch-num 1)
            (with-open-file (out (format nil "train-initial-conf-matrix-~d-~d-~d-~d.csv"
                                         *learning-rate*
                                         *momentum*
                                         *number-of-epochs*
                                         *number-hidden-activations*)
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
              (loop for i from 0 below 10 do
                    (loop for j from 0 below 10 do
                          (format out "~d," (aref *max-training-confusion-matrix* i j)))
                    (format out "~%"))
              (format out
                      "~d,~d,~d,~d"
                      *learning-rate*
                      *momentum*
                      *number-of-epochs*
                      *number-hidden-activations*)))

          ;; reset the training confusion matrix
          (setf *training-confusion-matrix* (make-array '(10 10) :initial-element 0))

          ;; Reset the number correct
          (setf num-correct 0)

          ;; Evaluate the accuracy of the testing data
          (loop for input across *testing-data* do
                (setf guess (classify input))
                (if (= guess (car input))
                  (incf num-correct))
                (incf (aref *testing-confusion-matrix* (car input) guess)))
          ;; Calculate the accuracy
          (setf *testing-accuracy* (float (/ num-correct *number-testing-images*)))
          ;; Keep track of the max testing accuracy
          (if (> *testing-accuracy* *max-testing-accuracy*)
            (progn (setf *max-testing-accuracy* *testing-accuracy*)
                   (setf *max-testing-confusion-matrix* (copy-array *testing-confusion-matrix*))))
          ;; Keep the 1st epoch's confusion matrix for funsies
          (if (= epoch-num 1)
            (with-open-file (out (format nil "test-initial-conf-matrix-~d-~d-~d-~d.csv"
                                         *learning-rate*
                                         *momentum*
                                         *number-of-epochs*
                                         *number-hidden-activations*)
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
              (loop for i from 0 below 10 do
                    (loop for j from 0 below 10 do
                          (format out "~d," (aref *max-testing-confusion-matrix* i j)))
                    (format out "~%"))
              (format out
                      "~d,~d,~d,~d~%"
                      *learning-rate*
                      *momentum*
                      *number-of-epochs*
                      *number-hidden-activations*)))

          ;; Reset the testing confusion matrix
          (setf *testing-confusion-matrix* (make-array '(10 10) :initial-element 0))


          ;; Append the accuracy data to the historic accuracies
          (vector-push-extend `(,epoch-num ,*training-accuracy* ,*testing-accuracy*) *historic-accuracies*)

          (princ (format nil "~%"))

          )
        )


  ;;; Output the training data to a file
  (with-open-file (out training-cm-filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (loop for i from 0 below 10 do
          (loop for j from 0 below 10 do
                (format out "~d," (aref *max-training-confusion-matrix* i j)))
          (format out "~%"))
    (format out
            "~d,~d,~d,~d"
            *learning-rate*
            *momentum*
            *number-of-epochs*
            *number-hidden-activations*))

  (with-open-file (out testing-cm-filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (loop for i from 0 below 10 do
          (loop for j from 0 below 10 do
                (format out "~d," (aref *max-testing-confusion-matrix* i j)))
          (format out "~%"))
    (format out
            "~d,~d,~d,~d~%"
            *learning-rate*
            *momentum*
            *number-of-epochs*
            *number-hidden-activations*))

  (with-open-file (out accuracies-filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format out "epoch,training accuracy,test accuracy")
    (loop for row across *historic-accuracies* do
          (format out "~{~a~^,~}~%" row))
    (format out
            "~%~d,~d,~d,~d~%"
            *learning-rate*
            *momentum*
            *number-of-epochs*
            *number-hidden-activations*)))


(with-open-file (in "trained-input-hidden-49.data" :direction :input :if-does-not-exist nil)
  (cond (in
          (setf *input-to-hidden-weight-matrix* (read in)))
        (t
          (break)
          (train-and-evaluate))))

(with-open-file (in "trained-hidden-output-49.data" :direction :input :if-does-not-exist nil)
  (if in (setf *hidden-to-output-weight-matrix* (read in))))

(defun validate-input (s i)
  (let ((actual nil)
        (guess nil))
    (cond ((eq s 'training)
           (setf actual (car (aref *training-data* i)))
           (setf guess (classify (aref *training-data* i))))
          ((eq s 'testing)
           (setf actual (car (aref *testing-data* i)))
           (setf guess (classify (aref *testing-data* i)))))
    (princ (format nil " Actual Digit: ~d~%" actual))
    (princ (format nil "Guessed Digit: ~d~%" guess))))





#|
| Todo list:
|  1) Test evaluate function
|X 2) Make 4 confusion matrices (training and testing data, initial and max)
|X 3) Make accuracy vars for training and testing data
|X 4) After each epoch evaluate accuracy of training and testing set
|X 5) Append accuracy to a vector #((epoch-num train-accuracy test-accuracy))
|X 6) After epoch 1, update initial and max confusion matrices for training
|     and testing sets
|X 7) After each epoch, if accuracy is greater than current max, overwrite max
|     confusion matrix for training and testing sets
|  8) Add timing outputs for each training epoch, the total training time, the
|     total time to classify a set.
|X 9) Output confusion matrices and accuracy after each epoch vector to a csv
|     file. Make sure the data includes the epoch number as well as the accuracy.
|X10) Create graphs either in jupyter or excel using the CSV files.
|
| Not enough time to implement the timing mechanism.
|#
