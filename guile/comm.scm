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
  (comm-pi world 10000000))

(define (display-my-id world)
  (display "comm = ")(display world)
  (display " rank = ")(display (comm-rank world))
  (display " of ")(display (comm-size world)))

;; Loop over ranks with communication barriers inbetween
;; for proper output formatting (does not always work):
(define (for-each-rank world proc)
  (let loop ((p 0) (size (comm-size world)))
    (if (< p size)
      (begin
        (if (= p (comm-rank world)) ; then it is my turn to act ...
          (begin
            ;;(display "comm = ")(display world)
            ;;(display " rank = ")(display p)
            ;;(display " of ")(display size)
            (display-my-id world)
            (display " ")
            (proc) ; lambda without args
            (newline)))
        (comm-barrier world) ; others wait here until I finish ...
        (loop (+ p 1) size)))))

(for-each-rank world (lambda () (display pi)))

;; each worker is either even or odd:
(define color (modulo rank 2))

;; group even and odd workers into communication groups:
(define country (comm-split world color))
(comm-set-name country (if (= color 0) "even" "odd"))
;; (display country)(newline)

;; let the two groups compete in computing the pi again:
(define pi-2 (comm-pi country 1000))

;; even output first, odd second:
(comm-barrier world)
(if (= color 0) (for-each-rank country (lambda () (display pi-2))))
(comm-barrier world)
(if (= color 1) (for-each-rank country (lambda () (display pi-2))))

;; release communicators:
(comm-free country)

(comm-barrier world)

;; ping-pong, FIXME: this will dead-lock with odd number of workers:
(let
  ((right (modulo (+ rank 1) size)) ; right peer
  (left (modulo (- rank 1) size)) ; left peer
  (token (+ rank 1000))
  (tag 999))
    (begin
      (if (= color 0) ; even send, odd receive ...
        (comm-send world right tag token)        ; on even
        (let ((ping (comm-recv world left tag))) ; on odd
          (display "rank = ")(display rank)
          (display " received ping ")(display ping)
          (newline)))
      (if (= color 1) ; even send, odd receive ...
        (comm-send world left tag token)          ; on odd
        (let ((pong (comm-recv world right tag))) ; on even
          (display "rank = ")(display rank)
          (display " received pong ")(display pong)
          (newline)))))

;; required by MPI:
(comm-finalize)

;; options for vim:sw=2:expandtab:smarttab:autoindent:syntax
