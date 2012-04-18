;;;
;;; Do not confuse with (baslib guile paragauss) in
;;;
;;; ./baslib/guile/paragauss.scm
;;;
(define-module (guile paragauss)
  #:export (*qm-trace-hook*
            qm-flush-trace))

(use-modules ((guile comm)
              #:select (comm-size
                        comm-rank)))

(use-modules (ice-9 pretty-print))      ; for qm-flush-trace only

;;
;; In  debug runs  here  we collect  a  list of  tracing entries,  see
;; *qm-trace-hook*, qm-flush-trace:
;;
(define *trace* '())

;;
;; This one is eventually called  from PG if the TRACE() macro exapnds
;; to something proper:
;;
(define (*qm-trace-hook* key file line time)
  "In some debug modes this funciton is called, if defined."
  ;; (display (list key file line time)) (newline) (force-output)
  ;;
  ;; Cons a new entry onto the global list:
  ;;
  (set! *trace* (cons (list key file line time) *trace*))
  0)                                    ; return something

;;
;; Dump trace entries collected during the run in *trace*:
;;
(define (qm-flush-trace world input output-dir)
  (if (not (null? *trace*))             ; dont create empty files
      (let* ((size (comm-size world))
             (rank (comm-rank world))
             (path (string-append output-dir "/"
                                  input "-"
                                  (number->string size) "-"
                                  (number->string rank))))
        ;;
        ;; Write the list of trace entries into a file:
        ;;
        (with-output-to-file
            path
          (lambda () (pretty-print (reverse *trace*))))
        ;;
        ;; Empty list of trace entries:
        ;;
        (set! *trace* '()))))
