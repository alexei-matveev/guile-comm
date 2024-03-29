#!/usr/bin/env guile-qm
!#

;;;
;;; Load  user-specific config, this  may also  set the  %load-path to
;;; search for files/modules.  This should work with both 1.8 and 2.0.
;;; Use  absolute paths  when  extending %load-path  ---  there is  no
;;; globbing for ~/ at compile time.
;;;
(cond-expand
 ((not guile-2) (use-modules (ice-9 syncase)))
 (else 'nothing)) ; eval-when for 1.8

(eval-when
 (eval load compile)
 (let ((conf (string-append (getenv "HOME") "/.qmrc")))
   (if (file-exists? conf)
       (load conf))))

;;
;; Home-grown modules. This needs %load-path to be extended e.g. as in
;;
;; (set! %load-path (cons "~/darcs/ttfs-mac" %load-path))
;;
;; It may be convenient to do this in ~/.qmrc.
;;
(use-modules ((guile comm)
              #:select
              (comm-bcast
               critical)))

;;
;; This  one  is  called  from se_scheduling_module  to  convert  MPTS
;; problem on behalf of the blocked egiensolver:
;;
(use-modules ((guile scheduling)
              #:select
              (qm-mpts->npts)))

;;
;; One of these, *qm-trace-hook*,  if defined/imported, is called from
;; scheme_trace_hook() in  PG.  So uncomment  it if you want  a trace.
;; The counterpart,  qm-flush-trace, will do  nothing if the  trace is
;; empty:
;;
(use-modules ((guile paragauss)
              #:select
              (; *qm-trace-hook*
               qm-flush-trace)))

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
;; Now that we are responsible for creating directories ourselves we
;; need to coordinate the work between process within and between SMP
;; hosts:
;;
(define (maybe-mkdir! world dirname) ; dirname is the same for all ranks
  (critical world
            (lambda ()
              (if (not (file-exists? dirname))
                  (mkdir dirname)))))

(define (maybe-rm-rf! world dirname) ; dirname is the same for all ranks
  (critical world
            (lambda ()
              (if (file-exists? dirname)
                  (system (string-append "rm -rf " dirname)))))) ; DANGEROUS !!!

;;
;; Call proc without args with a TTFSTMP environment variable pointing
;; to  a fresh temporary  directory on  each rank  in the  world.  Set
;; TTFSTMP to something different from $PWD to avoid polluting working
;; directory with temporary files before running PG:
;;
(define (with-temp-dir world dirname proc)
  (maybe-mkdir! world dirname)          ; make dir on each rank
  (setenv "TTFSTMP" dirname)            ; FIXME: not unset on exit
  (let ((result (proc)))                ; call proc, bind result
    (maybe-rm-rf! world dirname)        ; remove dir, DANGEROUS !!!
    result))                            ; return result of proc

;;
;; This actually runs an input:
;;
(define (run world input)
  (let ((temp-dir
         (comm-bcast world 0 (guess-temp-dir input))) ; prefer the value at rank 0
        (output-dir
         (comm-bcast world 0 (guess-output-dir input))))
    ;;
    ;; These are not undone later:
    ;;
    (setenv "TTFSINPUT" input)
    (setenv "TTFSOUTPUTDIR" output-dir)
    (maybe-mkdir! world output-dir)
    ;;
    ;; This creates temp-dir, runs the thunk and removes that dir:
    ;;
    (with-temp-dir world
                   temp-dir
                   (lambda () (qm-run world))) ; this invokes the program
    ;;
    ;; Dump trace log, if not empry, into a file:
    ;;
    (qm-flush-trace world input output-dir)
    ;;
    ;; Return total  enery, set in global namespace  by qm_run (), see
    ;; modules/paragauss.f90:
    ;;
    *energy*))

;;
;; Invoke  (program world)  with  the world  communicator privided  by
;; qm-init:
;;
(define (call-with-qm-world program)
  (let* ((world (qm-init))          ; intialize MPI, get communicator
         (result (program world)))  ; run the program, bind result
    (qm-finalize world)             ; finalize MPI
    result))                        ; return result

(define (main inputs)
  (define (program world)
    ;;
    ;; A program  that takes a communicator, processes  all inputs and
    ;; returns a list of results:
    ;;
    (map (lambda (x) (run world x)) inputs))
  ;;
  ;; Call the program with an MPI comm and return result:
  ;;
  (call-with-qm-world program))

;;
;; This expression is a list of energies, one per input:
;;
(main (cdr (command-line)))       ; first argument is the program name
