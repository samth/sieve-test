#lang typed/racket/base

;; Simple streams library.
;; For building and using infinite lists.

(provide make-stream
         stream-unfold
         stream-get
         stream-take
         stream)

;;--------------------------------------------------------------------------------------------------
(define-type stream (Pairof Natural (-> stream)))
(: make-stream (-> Natural (-> stream) stream))
(define (make-stream hd thunk)
  (cons hd thunk))

;; Destruct a stream into its first value and the new stream produced by de-thunking the tail
(: stream-unfold (-> stream (values Natural stream)))
(define (stream-unfold st)
  (values (car st) ((cdr st))))

;; [stream-get st i] Get the [i]-th element from the stream [st]
(: stream-get (-> stream Natural Natural))
(define (stream-get st i)
  (define-values (hd tl) (stream-unfold st))
  (cond [(= i 0) hd]
        [else    (stream-get tl (sub1 i))]))

;; [stream-take st n] Collect the first [n] elements of the stream [st].
(: stream-take (-> stream Natural (Listof Natural)))
(define (stream-take st n)
  (cond [(= n 0) '()]
        [else (define-values (hd tl) (stream-unfold st))
              (cons hd (stream-take tl (sub1 n)))]))
