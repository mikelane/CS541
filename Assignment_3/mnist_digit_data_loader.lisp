;;;;
;;;; MNIST Data Loader for Artificial Neural Networks written in LISP
;;;;
;;;; written by Mike Lane
;;;; 11/11/2016
;;;;
;;;; This module is used to load the MNIST Database of Handwritten Digits files
;;;; into a data structure that can be used for an Artificial Neural Network
;;;; classifier. You must first download the two training and two testing files
;;;; from the MNIST website, http://yann.lecun.com/exdb/mnist/. Once those files
;;;; are stored locally, you can import this file into your neural network
;;;; program and you can load the data into memory by doing something similar to
;;;; the following:
;;;;
;;;; (defparameter test-set
;;;;               (import-data "t10k-labels.idx1-ubyte"
;;;;                            "t10k-images.idx3-ubyte"))
;;;;
;;;; This example will load the test set information into memory as a lisp
;;;; vector where each element of the vector is a list. The list contains the
;;;; target as the car and the input activations, plus a bias, as the cadr.
;;;;
;;;; An example of the structue is:
;;;;
;;;; #((9 (1.0 0 0 0.75324 ... ))
;;;;   (3 (1.0 0 0.88274 0 ... ))
;;;;   ...
;;;;   (8 (1.0 0 0 0.01345 ... )))
;;;;
;;;; Be advised: There is a progress meter, but loading all the data takes quite
;;;; a long time.
;;;;
;;;;


#|
 | Read in the next 4 bytes from the file pointer so that each byte has a
 | width of 2. Then convert that into a decimal value and return it.
 |
 |#
(defun read-4-bytes (file-in)
  (read-from-string
    (format nil "#x~2,'0x~2,'0x~2,'0x~2,'0x"
            (read-byte file-in)
            (read-byte file-in)
            (read-byte file-in)
            (read-byte file-in))))


#|
 | Takes mnist csv file and makes a vector out of it that consists of lists with
 | the target value in the car and a list of the activations as the cdr. Returns
 | the number of images, the number of activations, and the data vector.
 |
 | (string string) => (num num #((num (num))))
 |
 |#
(defun import-data (labels-filename data-filename)
  (let ((ret (make-array 1 :adjustable t :fill-pointer 0))
        (label-magic-num 0)
        (image-magic-num 0)
        (num-images 0)
        (num-labels 0)
        (image-num-rows 0)
        (image-num-cols 0)
        (num-input-activations 0))
    (with-open-file (labels-in labels-filename
                               :direction :input
                               :element-type 'unsigned-byte)
      (with-open-file (data-in data-filename
                               :direction :input
                               :element-type 'unsigned-byte)
        (setf label-magic-num (read-4-bytes labels-in))
        (setf image-magic-num (read-4-bytes data-in))
        (setf num-labels (read-4-bytes labels-in))
        (setf num-images (read-4-bytes data-in))
        (setf image-num-rows (read-4-bytes data-in))
        (setf image-num-cols (read-4-bytes data-in))
        (setf num-input-activations (* image-num-rows image-num-cols))
        (princ (format nil "--------------------------------------------------------------------------~%"))
        (princ (format nil "Loading Labels File: ~s~%" labels-filename))
        (princ (format nil " - Magic Number: ~d Expected: 2049~%" label-magic-num))
        (princ (format nil " - Number of labels: ~d~%~%" num-labels))
        (princ (format nil "Loading Image File: ~s~%" data-filename))
        (princ (format nil " - Magic Number: ~d Expected: 2051~%" image-magic-num))
        (princ (format nil " - Number of images: ~d~%" num-images))
        (princ (format nil " - Image dimensions rows: ~d columns: ~d~%" image-num-rows image-num-cols))
        (princ (format nil " - Total number of input activations: ~d~%" num-input-activations))
        (princ (format nil "--------------------------------------------------------------------------~%"))
        (princ (format nil "~%Loading labels and data, merging, and pushing to a vector ...~%~%"))
        (princ (format nil "                       25%                      50%                      75%                      100%~%"))
        (princ (format nil "------------------------|------------------------|------------------------|------------------------|~%"))
        (let ((target 0)
              (data nil))
          (loop for i from 0 below num-labels do
                (setf target (read-from-string (format nil "#x~2,'0x" (read-byte labels-in))))
                (setf data
                      (append '(1.0)
                              (map 'list
                                   #'(lambda (x) (/ x 255.0))
                                   (loop for i from 0 below num-input-activations collect
                                         (read-from-string (format nil "#x~2,'0x" (read-byte data-in)))))))
                (cond ((> (fill-pointer ret) 0)
                       (vector-push-extend (list target data) ret))
                      (t (vector-push (list target data) ret)))
                (if (= 0 (mod i (/ num-labels 100)))
                  (princ (format nil "*")))))
        (princ (format nil "~%DONE~%~%"))
        )
      )
    `(,num-images ,num-input-activations ,ret)))

