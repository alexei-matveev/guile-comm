(use-modules (srfi srfi-1))             ; list manipulation
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

;;;
;;; These  are helpers  to  interprete the  data  in per-worker  files
;;; produced by test-eigensolver:
;;;
(define (get-dimensions pairs)
  (map car pairs))

(define (get-times pairs)
  (map cdr pairs))

(define (slurp path)
  (with-input-from-file path read))

(define (sum-all files)
  (let* ((data (map slurp files))
         (dims (let ((seq (delete-duplicates (map get-dimensions data))))
                 (if (= 1 (length seq))
                     (car seq)
                     (error "dimensions do not correspond" seq))))
         (times (map get-times data))
         (time (apply map + times)))
    (map cons dims time)))

(define (paste-all files)
  (let* ((data (map slurp files))
         (dims (let ((seq (delete-duplicates (map get-dimensions data))))
                 (if (= 1 (length seq))
                     (car seq)
                     (error "dimensions do not correspond" seq))))
         (times (map get-times data)))
    (cons dims times)))

(define (write-csv-row row)
  (display (string-join (map number->string row) ", "))
  (newline))

;; (pretty-print (sum-all (cdr (command-line))))
;; (pretty-print (paste-all (cdr (command-line))))

;; (let* ((files (cdr (command-line)))
;;       (data (paste-all files))
;;       (header (cons "n" files)))
;;   (write header)
;;   (newline)
;;   (for-each write-csv-row (apply map list data)))
