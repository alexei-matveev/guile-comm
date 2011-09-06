;(define libguile-comm (dynamic-link "./libguile-comm.so"))
;(dynamic-call "init_guile_comm" libguile-comm)

(display "A") (newline)

(define comm-world
    (comm-init (command-line)))

(display "B") (display (comm-rank)) (newline)

(display "comm-world") (display comm-world)

(comm-finalize)

(display "C") (newline)
