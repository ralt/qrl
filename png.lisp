(in-package #:qrl)

(defun png-draw (pixels pathname size)
  "Draws the pixels on the PNG file."
  (cl-gd:with-image* (size size)
    (cl-gd:allocate-color 255 255 255)
    (cl-gd:set-pixels pixels :color (cl-gd:allocate-color 0 0 0))
    (cl-gd:write-image-to-file pathname)))
