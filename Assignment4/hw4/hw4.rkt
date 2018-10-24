#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; these definitions are simply for the purpose of being able to run the tests
;; you MUST replace them with your solutions
;;

(define (sequence low high stride)
  (if (>= high low) 
      (cons low (sequence (+ low stride) high stride))
      null)
)

(define (string-append-map xs suffix)
  (map (lambda (x)
         (string-append x suffix)) xs )
  )


(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]  
        [(null? xs) (error "list-nth-mod: empty list")]  
        [#t (car(list-tail xs (remainder n (length xs))))])
  )

(define (stream-for-n-steps s n)
  (if (= 0 n)
      null
      (cons (car(s)) (stream-for-n-steps (cdr(s)) (- n 1)))
      )
  )

(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= 0 (remainder x 5))
                    (cons (* -1 x) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

(define cat-then-dog
  (letrec ([f (lambda (x)
                (if(string=? x "cat.jpg")
                 (cons "cat.jpg" (lambda () (f "dog.jpg")))
                 (cons "dog.jpg" (lambda () (f "cat.jpg")))))])
    (lambda () (f "cat.jpg"))))

;(define nats
;  (letrec ([f (lambda (x)
;                (cons x (lambda () (f (+ x 1)))))])
;    (lambda () (f 1))))

(define ones (lambda () (cons 1 ones)))

(define (stream-add-zero s)
  (letrec ([f (lambda ()
                (cons (cons 0 (car(s))) (lambda () (f))))])
    (lambda () (f))))

(define (cycle-lists xs ys)
  (letrec
      ([f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ 1 n)))))])
      (lambda () (f 0))
  )
)

;(vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1)))
(define (vector-assoc v vec)
  (letrec
      ([f (lambda (n) (if(= n (vector-length vec))
                         #f
                         (if(pair? (vector-ref vec n))
                            (if (equal? v (car (vector-ref vec n)))
                                 (vector-ref vec n)
                                 (f(+ n 1))
                             )
                            (f(+ n 1))
                         )
                      )
           )
        ]
       )
      (f 0)
   )
)

;(define (vector-assoc v vec)
;  (letrec
;      ([f (lambda (n) (if (= n vector-length vec)
;                         (#f)
;                         ((if (pair? (vector-ref vec n)) 
;                              (if(equal? (car (vector-ref vec n)) v)
;                                 (v)
;                                 (f(+ n 1))
;                              )
;                              (f(+ n 1))
;                         )
;                      )
;         ))]
;      )
;      (f 0)
;   )
;)

(define cached-assoc null)


