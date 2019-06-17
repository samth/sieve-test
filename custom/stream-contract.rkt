#lang racket/base
(require racket/contract/base)

;; Simple streams library.
;; For building and using infinite lists.

(define g7 (recursive-contract g15 #:chaperone))
(define g8 (recursive-contract g18 #:flat))
(define g9 (recursive-contract g21 #:chaperone))
(define g11 (flat-named-contract 'Natural exact-nonnegative-integer?))
(define g12 g9)
(define g13 (->* () () (values g12)))
(define g14 (or/c g11))
(define g15 (cons/c g14 g13))
(define g16 g8)
(define g17 (->* () () (values g16)))
(define g18 (cons/c g14 g17))
(define g19 g7)
(define g20 (->* () () (values g19)))
(define g21 (cons/c g14 g20))
(define-values (generated-contract3) (->* (g11 g13) () any))
(define-values (generated-contract4) (->* (g12 g11) () any))
(define-values (generated-contract5) (->* (g12 g11) () any))
(define-values (generated-contract6) (->* (g12) () any))


(provide (contract-out [make-stream generated-contract3]
                       [stream-unfold generated-contract6]
                       [stream-get generated-contract4]
                       [stream-take generated-contract5]))

;;--------------------------------------------------------------------------------------------------
;(define-type stream (Pairof Natural (-> stream)))
;(: make-stream (-> Natural (-> stream) stream))
(define (make-stream hd thunk)
  (cons hd thunk))

;; Destruct a stream into its first value and the new stream produced by de-thunking the tail
;(: stream-unfold (-> stream (values Natural stream)))
(define (stream-unfold st)
  (values (car st) ((cdr st))))

;; [stream-get st i] Get the [i]-th element from the stream [st]
;(: stream-get (-> stream Natural Natural))
(define (stream-get st i)
  (define-values (hd tl) (stream-unfold st))
  (cond [(= i 0) hd]
        [else    (stream-get tl (sub1 i))]))

;; [stream-take st n] Collect the first [n] elements of the stream [st].
;(: stream-take (-> stream Natural (Listof Natural)))
(define (stream-take st n)
  (cond [(= n 0) '()]
        [else (define-values (hd tl) (stream-unfold st))
              (cons hd (stream-take tl (sub1 n)))]))
