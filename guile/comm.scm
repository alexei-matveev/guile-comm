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
   critical))

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
;; Call  prog  with  for  each  rank  in  sequence  with  comm-barrier
;; inbetween:
;;
(define (critical world prog)
  (let loop ((rank 0))
    (if (< rank (comm-size world))
        (begin
          (if (= rank (comm-rank world)) ; My turn to ...
              (prog))                    ; ... execute prog.
          (comm-barrier world)           ; Others wait here.
          (loop (+ rank 1))))))
