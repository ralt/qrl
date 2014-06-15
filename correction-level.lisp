(in-package #:qrl)

;;; Since we only encode in byte mode, calculating the version and correction
;;; level is just related to the length of the text.
;;; The size if necessary to stop everything if the image can't hold
;;; enough pixels.
;;; We only support byte mode for now because it's the most useful one.
;;; If time and demand allows it, other modes will be implemented.


;;; The list of correction levels
(defconstant L 0) ;; 7%
(defconstant M 1) ;; 15%
(defconstant Q 2) ;; 25%
(defconstant H 3) ;; 30%

;;; Hard-coded list of the version/correction level/length values.
;;; Represented as: '((version . ((correction . length) (correction . length) [...])))
(defvar *character-capacities* '((1 . ((L . 17) (M . 14) (Q . 11) (H . 7)))
                                 (2 . ((L . 32) (M . 26) (Q . 20) (H . 14)))
                                 (3 . ((L . 53) (M . 42) (Q . 32) (H . 24)))
                                 (4 . ((L . 78) (M . 62) (Q . 46) (H . 34)))
                                 (5 . ((L . 106) (M . 84) (Q . 60) (H . 44)))
                                 (6 . ((L . 134) (M . 106) (Q . 74) (H . 58)))
                                 (7 . ((L . 154) (M . 122) (Q . 86) (H . 64)))
                                 (8 . ((L . 192) (M . 152) (Q . 108) (H . 84)))
                                 (9 . ((L . 230) (M . 180) (Q . 130) (H . 98)))
                                 (10 . ((L . 271) (M . 213) (Q . 151) (H . 119)))
                                 (11 . ((L . 321) (M . 251) (Q . 177) (H . 137)))
                                 (12 . ((L . 367) (M . 287) (Q . 203) (H . 155)))
                                 (13 . ((L . 425) (M . 331) (Q . 241) (H . 177)))
                                 (14 . ((L . 458) (M . 362) (Q . 258) (H . 194)))
                                 (15 . ((L . 520) (M . 412) (Q . 292) (H . 220)))
                                 (16 . ((L . 586) (M . 450) (Q . 322) (H . 250)))
                                 (17 . ((L . 644) (M . 504) (Q . 364) (H . 280)))
                                 (18 . ((L . 718) (M . 560) (Q . 394) (H . 310)))
                                 (19 . ((L . 792) (M . 624) (Q . 442) (H . 338)))
                                 (20 . ((L . 858) (M . 666) (Q . 482) (H . 382)))
                                 (21 . ((L . 929) (M . 711) (Q . 509) (H . 403)))
                                 (22 . ((L . 1003) (M . 779) (Q . 565) (H . 439)))
                                 (23 . ((L . 1091) (M . 857) (Q . 611) (H . 461)))
                                 (24 . ((L . 1171) (M . 911) (Q . 661) (H . 511)))
                                 (25 . ((L . 1273) (M . 997) (Q . 715) (H . 535)))
                                 (26 . ((L . 1367) (M . 1059) (Q . 751) (H . 593)))
                                 (27 . ((L . 1465) (M . 1125) (Q . 805) (H . 625)))
                                 (28 . ((L . 1528) (M . 1190) (Q . 868) (H . 658)))
                                 (29 . ((L . 1628) (M . 1264) (Q . 908) (H . 698)))
                                 (30 . ((L . 1732) (M . 1370) (Q . 982) (H . 742)))
                                 (31 . ((L . 1840) (M . 1452) (Q . 1030) (H . 790)))
                                 (32 . ((L . 1952) (M . 1538) (Q . 1112) (H . 842)))
                                 (33 . ((L . 2068) (M . 1628) (Q . 1168) (H . 898)))
                                 (34 . ((L . 2188) (M . 1722) (Q . 1228) (H . 958)))
                                 (35 . ((L . 2303) (M . 1809) (Q . 1283) (H . 983)))
                                 (36 . ((L . 2431) (M . 1911) (Q . 1351) (H . 1051)))
                                 (37 . ((L . 2563) (M . 1989) (Q . 1423) (H . 1093)))
                                 (38 . ((L . 2699) (M . 2099) (Q . 1499) (H . 1139)))
                                 (39 . ((L . 2809) (M . 2213) (Q . 1579) (H . 1219)))
                                 (40 . ((L . 2953) (M . 2331) (Q . 1663) (H . 1273)))))

(defun analyze-correction-level (text size)
  "Finds out what's the best version/correction level for the provided text."
  (let* ((version-level (calculate-version-level text))
         (version (car version-level)))
    (if (< size version)
        (signal 'error)
        version-level)))

(defun calculate-version-level (text)
  (do ((i (length *character-capacities*) (- i 1)))
      ((= i 0))
    (let* ((char-cap (nth i *character-capacities*))
           (version (car char-cap))
           (corrections (cdr char-cap)))
      (do ((j 0 (1+ j)))
          ((= j (length corrections)))
        (when (> (length text) (cdr (nth j corrections)))
          (return-from calculate-version-level (list version (car (nth (- j 1) corrections)))))))))
