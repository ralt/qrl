;;;; qrl.asd

(asdf:defsystem #:qrl
  :serial t
  :description "QR Code encoder in Lisp"
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :depends-on ("cl-gd"
               "alexandria")
  :components ((:file "package")
               (:file "qrl")
               (:file "correction-level")
               (:file "bits-encoder")
               (:file "pixels")
               (:file "png")))
