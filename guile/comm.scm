;(define libguile-comm (dynamic-link "./libguile-comm.so"))
;(dynamic-call "init_guile_comm" libguile-comm)

(define world
    (comm-init (command-line)))

(display "world=") (display world) (newline)
(display "rank=") (display (comm-rank world)) (newline)
(display "size=") (display (comm-size world)) (newline)

(define pi
    (comm-pi world 1000000000))
(display "pi=") (display pi) (newline)

(comm-finalize)
