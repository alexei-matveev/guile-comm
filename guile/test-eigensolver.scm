(use-modules (ice-9 pretty-print))

;;;
;;; This section  makes use of the COMM-  and QM-extensions comm-rank,
;;; comm-size, and qm-test-eigensolver interpreter and will not run in
;;; plain Guile:
;;;
(define (test-eigensolver dimensions)
  ;;
  ;; Calls parallel eigensolver for  a series of dimensions and writes
  ;; the timings into a series of per-worker files.
  ;;
  (let* ((world (qm-init))
         (np (comm-size world))
         (rank (comm-rank world))
         (times (map qm-test-eigensolver dimensions))
         (pairs (map cons dimensions times))
         (path (string-append "series-"
                              (number->string np)
                              "-"
                              (number->string rank))))
    (with-output-to-file path
      (lambda () (pretty-print pairs)))
    ;; (pretty-print (cons rank pairs))
    (qm-finalize world)))

;;
;; List of matrix dimension to try eigensolver with:
;;
(define dimensions
  (map (lambda (x) (* x x)) (iota 30)))

(test-eigensolver dimensions)
