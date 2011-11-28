#!/usr/bin/env guile-qm
!#

(use-modules (ice-9 syncase))		; syntax-rules

;;
;; This is artificial intelligence guessing temp dir:
;;
(define (guess-temp-dir input)
  (let ((prefix (or (getenv "SCRATCH") ; set on SuperMUC
                    (getenv "OPT_TMP") ; used by LRZ
                    (getenv "TEMP")    ; not sure if ever used
                    "/scratch")))      ; if none is set use this
    (string-append prefix "/" (getenv "USER") "-"
		   (input-base-name input) "-"
		   (number->string (getpid)))))

;;
;; This emulates the behaviour of the "runpg" bash script that tells
;; PG to put output into o.name/ for an input i.name:
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
;; Evaluate one or more expression for each rank in sequence with
;; comm-barrier inbetween (critical world expr1 expr2 ...) to be
;; compared with (begin expr1 expr2 ...)
;;
(define-syntax critical
  (syntax-rules ()
    ((critical world expr1 expr2 ...)
     (let loop ((rank 0)) ; quote this sexp with ' to check the transcription
       (if (< rank (comm-size world))
	   (begin
	     (if (= rank (comm-rank world)) ; My turn to ...
		 (begin expr1 expr2 ...))   ; ... evaluate expresssions.
	     (comm-barrier world))	    ; Others wait here.
	     (loop (+ rank 1)))))))

;;
;; Now that we are responsible for creating directories ourselves we
;; need to coordinate the work between process within and between SMP
;; hosts:
;;
(define (maybe-mkdir! world dirname) ; dirname is the same for all ranks
  (critical world
	    (if (not (file-exists? dirname))
		(mkdir dirname))))

(define (maybe-rm-rf! world dirname) ; dirname is the same for all ranks
  (critical world
	    (if (file-exists? dirname)
		(system (string-append "rm -rf " dirname))))) ; DANGEROUS !!!

;;
;; Set TTFSTMP to something different from $PWD to avoid polluting
;; working directory with temporary files before running PG:
;;
(define (run world input)
  (let
      ((temp-dir (comm-bcast world 0 (guess-temp-dir input))) ; prefer the value at rank 0
       (output-dir (comm-bcast world 0 (guess-output-dir input))))
    (begin
      (setenv "TTFSINPUT" input)
      (setenv "TTFSOUTPUTDIR" output-dir)
      (setenv "TTFSTMP" temp-dir)	; dont ask in guess-temp-dir
      (maybe-mkdir! world temp-dir)	; create temp-dir
      (maybe-mkdir! world output-dir)
      (qm-run world)			; this invokes the program
      (maybe-rm-rf! world temp-dir))))	; remove temp-dir, DANGEROUS !!!

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
	(run world (car inputs))	; this invokes the program
	(loop (cdr inputs)))))

;;
;; No more communication after that:
;;
(qm-finalize world)

;; Default options for vim:sw=2:expandtab:smarttab:autoindent:syntax=scheme
