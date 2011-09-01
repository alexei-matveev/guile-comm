;(define libguile-comm (dynamic-link "./libguile-comm.so"))
;(dynamic-call "init_guile_comm" libguile-comm)

(display "A") (newline)

(comm-init (command-line))

(display "B") (display (comm-rank)) (newline)

(comm-finalize)

(display "C") (newline)
