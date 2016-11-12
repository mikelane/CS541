;;;;
;;;; Michael Lane
;;;; CS541 - Artificial Intelligence
;;;; Lab 3
;;;; Main Artificial Neural Network
;;;;

(load "nn_utils.fas")


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
