#lang racket/base
(require racket/performance-hint racket/contract/base)
;; Simple streams library.
;; For building and using infinite lists.

(define stream/c/rec (recursive-contract stream/c #:chaperone))
(define stream/c (cons/c exact-nonnegative-integer? (-> stream/c/rec)))

#;(provide (contract-out [make-stream (->* (exact-nonnegative-integer? (-> stream/c/rec)) () any)]
                       [stream-unfold (->* (stream/c/rec) () any)]
                       [stream-get (->* (stream/c/rec exact-nonnegative-integer?) () any)]
                       [stream-take (->* (stream/c/rec exact-nonnegative-integer?) () any)]))

(provide (rename-out [stream-take* stream-take]
                     [make-stream* make-stream]
                     [stream-get* stream-get]
                     [stream-unfold* stream-unfold]))

(define-inline (wrap-pair p who)
  (unless (and (pair? p)
               (exact-nonnegative-integer? (car p)))
    (error who))
  (cons (car p) (wrap-thunk (cdr p) who)))

(define (wrap-thunk/proc thunk who)
  (unless (and (procedure? thunk) (procedure-arity-includes? thunk 0))
    (error who))
  (lambda ()
    (define s (thunk))
    (wrap-pair s 'who)))

(define (wrap-thunk/chap thunk who)
  (unless (and (procedure? thunk) (procedure-arity-includes? thunk 0))
    (error who))
  (chaperone-procedure thunk (lambda () (lambda (s) (wrap-pair s 'xxx)))))

(define wrap-thunk wrap-thunk/proc)

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
(define-inline (stream-unfold st)
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
