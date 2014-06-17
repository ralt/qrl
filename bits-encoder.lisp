(in-package #:qrl)

(defun bits-encode (text correction-level)
  "Encodes text in bits, according to the correction level."
  (let ((version (car correction-level))
        (correction (cdr correction-level)))
    (alexandria:flatten (list (mode-indicator)
                              (char-count-indicator text version)
                              (text-encode text)
                              (error-correction-codewords correction)))))


(defconstant byte-mode 0100)

(defun mode-indicator ()
  "Mode indicators bits"
  (loop for c across (write-to-string byte-mode) collect c))

;;;; For versions 1 to 9, 8 bits
(defvar *byte-mode-length* (make-hash-table :test 'equal))

(defmacro defbytemode (length versions)
  `(dolist (v ,versions)
     (setf (gethash v *byte-mode-length*) ,length)))

(defbytemode 8
    '(1 2 3 4 5 6 7 8 9))

(defbytemode 16
    '(10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26))

(defbytemode 16
    '(27 28 29 30 31 32 33 34 35 36 37 38 39 40))

(defun char-count-indicator (text version)
  "Length of text indicator. Number of bits depends on the version."
  (let* ((number-of-bits-length (gethash version *byte-mode-length*))
         (binary-length (loop for c across (format nil "~b" (length text)) collect c)))
    ;; Needs padding with 0
    (loop for i from 0 to (- number-of-bits-length (length binary-length) 1)
         do (setf binary-length (cons #\0 binary-length)))
    binary-length))

(defun text-encode (text)
  "Encodes the text in bits.")

(defun error-correction-codewords (correction)
  "Calculates the error correction codewords.")
