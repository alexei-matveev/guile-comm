;;
;; Module/package systems differ between implementaitons:
;;
(cond-expand
 (guile ; primary implementation
  (use-modules (ice-9 pretty-print))
  (use-modules (srfi srfi-1)))  ; list manipulations
 (else  ; mzscheme, aka PLT Scheme, aka Racket (needs cond-expand
        ; macro in ~/.mzschmerc):
  (require (lib "1.ss" "srfi"))
  (define (1+ x) (+ 1 x))
  (define (sorted? lst pred?) #t)))     ; FIXME: lies!

;;;
;;; See "Scheduling malleable and nonmalleable parallel tasks", Walter
;;; Ludwig, Prasoon  Tiwari, SODA '94 Proceedings of  the fifth annual
;;; ACM-SIAM symposium on Discrete algorithms.
;;;

;;
;; Task  is  an "abstract"  data  type  (currently  represented by  an
;; integer size parameter  of the task).  And "partition"  is a subset
;; of workers  (currently represented by an integer  worker count). It
;; is assumed the two are  sufficient to determine the cumulative work
;; function:
;;
(define (make-world size) size)
(define (make-task size) size)
(define (world-size world) world)
(define (task-size task) task)
;; (define (make-world size) (cons 'world size))
;; (define (make-task size) (cons 'task size))
;; (define (world-size world) (cdr world))
;; (define (task-size task) (cdr task))

(define (work-function task world)
  ;;
  ;; Work function should better be a non-decreasing function of
  ;; number of workers:
  ;;
  (let ((n (task-size task))
        (p (world-size world)))
    (+ (* n n n)                        ; O(n**3) work
       (* n n (- p 1)))))               ; O(pn**2) overhead

;; (pretty-print
;;  (work-function (make-task 100) (make-world 16)))

;;
;; We may use these properties for optimizations, like binary search
;; on bounds:
;;
(define (non-decreasing? lst) (sorted? lst <))
(define (non-increasing? lst) (sorted? lst >))
(define (regular-case? partitions time-matrix time-series)
  (let* ((sizes
          (map world-size partitions))
         (load-matrix
          (map (lambda (times) (map * times sizes))
               time-matrix)))
    (and (non-decreasing? sizes)
         (non-decreasing? time-series)
         (every non-increasing? time-matrix)
         (every non-decreasing? load-matrix))))

(define (feasible-partitions world)
  ;;
  ;; Returns a list of feasible subsets of workers to execute the
  ;; task. Here all feasible partitions of the size ranging from one
  ;; to the number of workers (FIXME: partition as a function of size
  ;; only here, feasible partitions do not depend on task):
  ;;
  (let* ((size (world-size world))
         (sizes (map 1+ (iota size))))
    (map make-world sizes)))

;; (pretty-print
;;  (feasible-partitions (make-world 10)))

;;
;; Computes f(a, b) for all pairs from the list of a's and the list of
;; b's. Returns a nested list [[f(a, b) | b <- bs] | a <- as]:
;;
(define (outer f as bs)
  (map (lambda (a) (map (lambda (b) (f a b)) bs)) as))

;; (pretty-print
;;  (outer cons '(1 2 3) '(a b c d)))

;;
;; Returns #f if any of evaluations of maybe-proc do so:
;;
(define (maybe-map maybe-proc args)
  (let loop ((args args)
             (acc '()))
    (if (null? args)
        (reverse acc)
        (let ((res (maybe-proc (car args))))
          (and res
               (loop (cdr args) (cons res acc)))))))

;; (define (test n)
;;   (if (= 0 n)
;;       #f
;;       (/ 1 n)))

;; (pretty-print
;;  (maybe-map test '(1 2 3 4)))

;; (pretty-print
;;  (maybe-map test '()))

;; (pretty-print
;;  (maybe-map test '(1 2 0 4)))

(define (maybe-find threshold candidates cost size)
  ;;
  ;; May return  #f if the list of candidates is  empty or rather when
  ;; there   is  no  candidate   with   a  cost  below   or  equal  to
  ;; threshold. Otherwise return one of the candidates of the smallest
  ;; size with the cost not above threshold.
  ;;
  ;; O(P) complexity, with P being the number of worker candidates.
  ;;
  (let ((choose (lambda (new old)
                  ;;
                  ;; Check if the new is "more appropriate" than the
                  ;; old, return one or another depending on that:
                  ;;
                  (if (> (cost new) threshold) ; filter them out
                      old
                      (if (or (not old) ; not #f, only then compare sizes:
                              (< (size new) (size old)))
                          new
                          old)))))
    (fold choose #f candidates)))

(define (mpts->npts work-function tasks world)
  ;;
  ;; Suggest a partition (number  of workers) for each of three tasks.
  ;; Expects  a list of task  descriptors and a desriptor  of a worker
  ;; set.
  ;;
  (let* ((np
          (world-size world))

         (mean-work
          (lambda (tasks partitions)
            (let* ((loads (map work-function tasks partitions))
                   (total (apply + loads)))
              (/ total np))))

         (cost-function
          (lambda (task world)
            ;;
            ;; Time span is a total work divided by number of workers
            ;; (and conversly). Should better be a non-increasing
            ;; function of number of workers:
            ;;
            (let ((w (work-function task world))
                  (p (world-size world)))
              (/ w p))))

         (partitions
          (feasible-partitions world))

         ;;
         ;; Costs of every task on every partition. O(MP) size.
         ;;
         (time-matrix
          (outer cost-function tasks partitions))

         (time-series
          (delete-duplicates (sort (concatenate time-matrix) <)))

         ;;
         ;; This function finds one subset of the world of the
         ;; smallest size that allows execution of the task in time
         ;; below tau.  In case it does not, it returns #f.  O(P)
         ;; complexity inherited from maybe-find.
         ;;
         (maybe-find-partition
          (lambda (task tau)
            (let ((cost (lambda (world) (cost-function task world)))) ; cost as func of partition
              (maybe-find tau partitions cost world-size))))

         ;;
         ;; This function finds one subset of the world of the
         ;; smallest size that allows execution of the task in time
         ;; below tau. O(MP) complexity with M being the number of
         ;; tasks.
         ;;
         (maybe-find-partitions
          (lambda (tau)
            (maybe-map (lambda (task)
                         (maybe-find-partition task tau))
                       tasks)))

         ;;
         ;; O(MP) complexity, with M being the number of tasks and
         ;; P being the number of workers, inherited from
         ;; maybe-find-partitions.
         ;;
         (maybe-work
          (lambda (tau)
            (let ((partitions (maybe-find-partitions tau)))
              (and partitions (mean-work tasks partitions)))))

         ;;
         ;; O(MP), inherited from maybe-work.
         ;;
         (maybe-bound
          (lambda (tau)
            (let ((w (maybe-work tau)))
              (and w (max tau w))))))

    ;; (pretty-print time-series)
    (if (regular-case? partitions time-matrix time-series)
        'do-a-binary-search-maybe?
        (error "not sorted, check partitions and cost function"))

    ;;
    ;; O((MP)**2) complexity, the product of that for maybe-bound and
    ;; the size of time-series.
    ;;
    (let* ((bounds (filter-map maybe-bound time-series))
           (omega (apply min bounds))
           (optimal (maybe-find-partitions omega))
           (times (map cost-function tasks optimal)))
      (list tasks '@ world '-> optimal times))))

;; (define (maybe-bisect f a b)
;;   (let search ((a a)
;;                (b b)
;;                (fa (f a))
;;                (fb (f b)))
;;     (if (= a b)
;;         a
;;         (let ((c (quotient (+ a b) 2))
;;               (fc (f c)))
;;           (if )))))

;; (pretty-print
;;  (mpts->npts work-function
;;              (map make-task '(0 10 100))
;;              (make-world 4)))

(pretty-print
 (mpts->npts work-function
             (map make-task '(50 50 50 50 50 100 100 100 100 100))
             (make-world 40)))
