#!/usr/bin/env guile-qm
!#

;;
;; This is artificial intelligence guessing temp dir:
;;
(define (guess-temp-dir input)
  (let ((prefix (or (getenv "TTFSTMP") ; returns the first that is set ...
                    (getenv "SCRATCH") ; set on SuperMUC
                    (getenv "OPT_TMP") ; used by LRZ
                    (getenv "TEMP")    ; not sure if ever used
                    "/scratch")))      ; if none is set use this
    (string-append prefix "/" (getenv "USER") "-"
		   (input-base-name input) "-"
		   (number->string (getpid)))))

;;
;; This emulates the behaviour of the "runpg" bash script
;; that tells PG to put output into o.name/ for an input i.name:
;;
(define (guess-output-dir input)
  (cond
    ((string=? "input" input) ".") ; for input named input use CWD
    (#t (string-append "o." (input-base-name input))))) ; by default use o.$name for output

(define (input-base-name input)
  (cond
    ((string-prefix? "i." input) (string-drop input 2))
    (#t input)))

;;
;; Now that we are responsible for creating directories ourselves
;; we need to coordinate the work between process within and between
;; SMP hosts:
;;
(define (maybe-mkdir! world dirname)
  (let ((dirname (comm-bcast world 0 dirname))) ; prefer the value at rank 0
    (let loop ((rank 0))
      (if (< rank (comm-size world))
        (begin
          (if (= rank (comm-rank world)) ; my turn to act ...
            (if (not (file-exists? dirname))
              (mkdir dirname)))
          (comm-barrier world) ; others wait here, till I finish with mkdir ...
          (loop (+ rank 1)))))))

;;
;; Set TTFSTMP to something different from $PWD to avoid
;; polluting working directory with temporary files
;; before running PG:
;;
(define (run world input)
  (let
    ((temp-dir (comm-bcast world 0 (guess-temp-dir input))) ; prefer the value at rank 0
    (output-dir (comm-bcast world 0 (guess-output-dir input))))
      (begin
        (setenv "TTFSINPUT" input)
        (setenv "TTFSOUTPUTDIR" output-dir)
        (setenv "TTFSTMP" temp-dir)
        (maybe-mkdir! world temp-dir) ; it case it isnt, create it, on all workes
        (maybe-mkdir! world output-dir)
        (qm-run world) ; this invokes the program
        (system (string-append "echo Dont forget to rm -rf " temp-dir)))))
        ; FIXME: do "rm -rf" when confident

;;
;; Intialize MPI, get the world communicator:
;;
(define world (qm-init))

;;
;; Actually run the program for all inputs in the command line:
;;
(let loop ((inputs (cdr (command-line)))) ; first argument is the program name
  (if (not (null? inputs))
    (begin
      (run world (car inputs)) ; this invokes the program
      (loop (cdr inputs)))))

;;
;; No more communication after that:
;;
(qm-finalize world)

;; Default options for vim:sw=2:expandtab:smarttab:autoindent:syntax=scheme
