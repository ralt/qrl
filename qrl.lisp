;;;; qrl.lisp

(in-package #:qrl)

;;; "qrl" goes here. Hacks and glory await!

(defun encode (text pathname size)
  "Encodes a text in a QR Code, and creates a PNG file at the specified filename."
  (let ((correction-level (analyze-correction-level text size)))
    (png-draw (pixels-draw (bits-encode text correction-level)) pathname size)))
