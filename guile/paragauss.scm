;;;
;;; Do not confuse with (baslib guile paragauss) in
;;;
;;; ./baslib/guile/paragauss.scm
;;;
(define-module (guile paragauss)
  #:export (with-stdout-to-file
            *qm-trace-hook*
            qm-flush-trace))

(use-modules ((guile comm)
              #:select (comm-size
                        comm-rank)))

(use-modules (ice-9 pretty-print))      ; for qm-flush-trace only

;;;
;;; Beware that writing to the  same file from multiple workers is not
;;; going to end good:
;;;
(define (with-stdout-to-file file proc)
  (with-output-to-file file
    (lambda ()
      (let ((dup-1 (dup 1))         ; make a dup of the current stdout
            (fd (fileno (current-output-port))))
        (dup2 fd 1)     ; close stdout and redirect it to fd
        (proc)          ; execute thunk with stdout redirected to file
        (dup2 dup-1 1)  ; close file, stdout to original destination
        (close dup-1)))))               ; dont leak file descriptors

;; (with-stdout-to-file "a1"
;;   (lambda ()
;;     (display "Hi!")
;;     (newline)))


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
