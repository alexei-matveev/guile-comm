;(define libguile-comm (dynamic-link "./libguile-comm.so"))
;(dynamic-call "init_guile_comm" libguile-comm)

(display "before") (newline)

(define world
    (comm-init (command-line)))

(display "world=") (display world) (newline)

(display "rank=") (display (comm-rank world)) (newline)
(display "size=") (display (comm-size world)) (newline)

(comm-finalize)

(display "after") (newline)
