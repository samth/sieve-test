#lang racket/base

(require (prefix-in p: (submod "stream-handcoded.rkt" plain)))
(require (prefix-in w: (submod "stream-handcoded.rkt" wrapped)))
(require (prefix-in c: (submod "stream-handcoded.rkt" contracted)))

;;--------------------------------------------------------------------------------------------------

(define-syntax-rule (go make-stream stream-unfold stream-get)
  (let ()
    ;; `count-from n` Build a stream of integers starting from `n` and iteratively adding 1
    ;(: count-from (-> Natural stream))
    (define (count-from n)
      (make-stream n (lambda () (count-from (add1 n)))))

    ;; `sift n st` Filter all elements in `st` that are equal to `n`.
    ;; Return a new stream.
    ;(: sift (-> Natural stream stream))
    (define (sift n st)
      (define-values (hd tl) (stream-unfold st))
      (cond [(= 0 (modulo hd n)) (sift n tl)]
            [else (make-stream hd (lambda () (sift n tl)))]))

    ;; `sieve st` Sieve of Eratosthenes
    ;(: sieve (-> stream stream))
    (define (sieve st)
      (define-values (hd tl) (stream-unfold st))
      (make-stream hd (lambda () (sieve (sift hd tl)))))

    ;; stream of prime numbers
    ;(: primes stream)
    (define primes (sieve (count-from 2)))

    ;(: N-1 Natural)
    (define N-1 666)

    ;(: main (-> Void))
    (define (main)
      (printf "the ~s prime is ~s\n" N-1 (stream-get primes N-1)))
    main))

(define p:main (go p:make-stream p:stream-unfold p:stream-get))
(define w:main (go w:make-stream w:stream-unfold w:stream-get))
(define c:main (go c:make-stream c:stream-unfold c:stream-get))

(collect-garbage)
'plain
(time (p:main))
(collect-garbage)
'hand-coded
(time (w:main))
(collect-garbage)
'contract
(time (c:main))
