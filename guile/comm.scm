;(define libguile-comm (dynamic-link "./libguile-comm.so"))
;(dynamic-call "init_guile_comm" libguile-comm)

;; returns MPI_COMM_WORLD:
(define world
    (comm-init (command-line)))

;; size and rank are communicator specific:
(define size (comm-size world))
(define rank (comm-rank world))

;; compute PI in parallel:
(define pi
    (comm-pi world 1000000000))

;; Loop over ranks with communication barriers inbetween
;; for proper output formatting:
(let loop ((p 0))
    (if (< p size)
        (begin
            (if (equal? rank p)
                (begin
                    (display "comm = ") (display world)
                    (display " rank = ") (display p)
                    (display " of ") (display size)
                    (display " pi = ") (display pi)
                    (newline)))
            (comm-barrier world)
            (loop (+ p 1)))))

;; required by MPI:
(comm-finalize)
