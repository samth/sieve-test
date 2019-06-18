#lang racket/base
(require racket/performance-hint racket/contract/base racket/unsafe/ops)
;; Simple streams library.
;; For building and using infinite lists.

(define stream/c/rec (recursive-contract stream/c #:chaperone))
(define stream/c (cons/c exact-nonnegative-integer? (-> stream/c/rec)))

(module* contracted #f
  (provide (contract-out [make-stream (->* (exact-nonnegative-integer? (-> stream/c/rec)) () any)]
                         [stream-unfold (->* (stream/c/rec) () any)]
                         [stream-get (->* (stream/c/rec exact-nonnegative-integer?) () any)]
                         [stream-take (->* (stream/c/rec exact-nonnegative-integer?) () any)])))

(module* wrapped #f
  (provide (rename-out [stream-take* stream-take]
                       [make-stream* make-stream]
                       [stream-get* stream-get]
                       [stream-unfold* stream-unfold])))
(module* plain #f
  (provide stream-take stream-get stream-unfold make-stream))

(define-inline (wrap-pair p who)
  (unless (and (pair? p)
               (exact-nonnegative-integer? (car p)))
    (error who))
  (cons (car p) (wrap-thunk (cdr p) who)))

(define (wrap-thunk/proc thunk who)
  (unless (and (procedure? thunk) (procedure-arity-includes? thunk 0))
    (error who))
  (lambda ()
    (call-with-immediate-continuation-mark
     'contract-cont-mark
     (lambda (m)
       (if (and m (eq? (car m) 'stream) (eq? (cdr m) who))
           (thunk)
           (wrap-pair (with-continuation-mark 'contract-cont-mark (cons 'stream who) (thunk)) who))))))

(define (wrap-thunk/chap thunk who)
  (unless (and (procedure? thunk) (procedure-arity-includes? thunk 0))
    (error who))
  ;(eprintf "chaperoning!\n")
  (chaperone-procedure thunk (lambda () (lambda (s) (wrap-pair s 'xxx)))))

(define (wrap-thunk/unsafe-chap thunk who)
  (unless (and (procedure? thunk) (procedure-arity-includes? thunk 0))
    (error who))
  (unsafe-chaperone-procedure thunk (wrap-thunk/proc thunk who)))

(define-syntax-rule (wrap-thunk . args) (wrap-thunk/proc . args))

(define-inline (make-stream* hd thunk)
  (unless (exact-nonnegative-integer? hd)
    (error 'make-stream-client))
  (make-stream hd (wrap-thunk thunk 'make-stream-client)))

;;--------------------------------------------------------------------------------------------------
;(define-type stream (Pairof Natural (-> stream)))
;(: make-stream (-> Natural (-> stream) stream))
(define-inline (make-stream hd thunk)
  (cons hd thunk))

;; Destruct a stream into its first value and the new stream produced by de-thunking the tail
;(: stream-unfold (-> stream (values Natural stream)))
(define-inline (stream-unfold* st)
  (stream-unfold (wrap-pair st 'stream-unfold-client)))
(define (stream-unfold st)
  (values (car st) ((cdr st))))

;; [stream-get st i] Get the [i]-th element from the stream [st]
;(: stream-get (-> stream Natural Natural))
(define-inline (stream-get* st i)
  (unless (exact-nonnegative-integer? i)
    (error 'stream-get-client))
  (stream-get (wrap-pair st 'stream-get-client) i))

(define (stream-get st i)
  (define-values (hd tl) (stream-unfold st))
  (cond [(= i 0) hd]
        [else    (stream-get tl (sub1 i))]))

;; [stream-take st n] Collect the first [n] elements of the stream [st].
;(: stream-take (-> stream Natural (Listof Natural)))
(define-inline (stream-take* st i)
  (unless (exact-nonnegative-integer? i)
    (error 'stream-take-client))
  (stream-take (wrap-pair st 'stream-take-client) i))

(define (stream-take st n)
  (cond [(= n 0) '()]
        [else (define-values (hd tl) (stream-unfold st))
              (cons hd (stream-take tl (sub1 n)))]))
