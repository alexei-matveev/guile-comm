(define-module (guile comm)
  #:export
  (comm-init
   comm-finalize
   comm-rank
   comm-size
   comm-barrier
   comm-bcast
   comm-send-recv
   comm-send
   comm-recv
   comm-split
   comm-free
   comm-set-name
   comm-pi
   critical))                           ; syntax

(use-modules (ice-9 syncase))           ; syntax-rules

;;
;; This name has to be defined  on guile startup, see the C sources of
;; guile_main ():
;;
(define guile-comm-module-init
  (@@ (guile-user) guile-comm-module-init))

;;
;; This C-code defines all of the exported symbols:
;;
(guile-comm-module-init)

;;
;; Evaluate  one or  more expression  for each  rank in  sequence with
;; comm-barrier  inbetween  (critical world  expr1  expr2  ...) to  be
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
             (comm-barrier world)           ; Others wait here.
             (loop (+ rank 1))))))))
