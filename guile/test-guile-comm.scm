;(define libguile-comm (dynamic-link "./libguile-comm.so"))
;(dynamic-call "guile_comm_init_module" libguile-comm)

;; returns MPI_COMM_WORLD:
(define world
  (comm-init (command-line)))

;; size and rank are communicator specific:
(define size (comm-size world))
(define rank (comm-rank world))

;;
;; Cyclic shift, send to the right, receive from the left:
;;
(let
  ((right-peer (modulo (+ rank 1) size))
  (left-peer (modulo (- rank 1) size))
  (token (list "token of rank" rank "with payload" (+ rank 0.1))) ; token to pass, rank specific
  (message-tag 999)) ; arbitrary number, same for every worker
    (begin
      (let loop ((i 0) (token token))
        (if (< i size)
          (let
            ((token (comm-send-recv world right-peer left-peer message-tag token)))
              (begin
                (display (list rank token))(newline)
                (loop (+ i 1) token)))))))

;;
;; Compute PI in parallel (by calling native code with our communicator):
;;
(define pi
  (comm-pi world 10000000))

;;
;; Loop over ranks with communication barriers inbetween
;; for proper output formatting (does not always work):
;;
(define (for-each-rank world proc)
  (let loop ((p 0) (size (comm-size world)))
    (if (< p size)
      (begin
        (if (= p (comm-rank world)) ; then it is my turn to act ...
          (begin
            (let ((result (proc)))
              (display (list world (comm-rank world) (comm-size world) result)))
            (newline)))
        (comm-barrier world) ; others wait here until I finish ...
        (loop (+ p 1) size)))))

(for-each-rank world (lambda () pi))

; (exit 0)

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
(if (= color 0) (for-each-rank country (lambda () pi-2)))
(comm-barrier world)
(if (= color 1) (for-each-rank country (lambda () pi-2)))

;; release communicators:
(comm-free country)

(comm-barrier world)

;;
;; FIXME: ugly abstraction:
;;
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
          (display (list rank "recv ping" ping))
          (newline)))
      (if (= color 1) ; odd send, even receive ...
        (comm-send world left tag token)          ; on odd
        (let ((pong (comm-recv world right tag))) ; on even
          (display (list rank "recv pong" pong))
          (newline)))))

;; required by MPI:
(comm-finalize)

;; options for vim:sw=2:expandtab:smarttab:autoindent:syntax
