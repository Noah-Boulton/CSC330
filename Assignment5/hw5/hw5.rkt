;; Programming Languages, Homework 5 version 1.1
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Problem A

;; CHANGE (put your solutions here)
(define (mupllist->racketlist lst)
  (if (aunit? lst)
      null
      (cond [(apair? (apair-e1 lst)) (cons (mupllist->racketlist (apair-e1 lst)) (mupllist->racketlist (apair-e2 lst)))]
            [#t (cons (apair-e1 lst) (mupllist->racketlist (apair-e2 lst)))])))

(define (racketlist->mupllist lst)
  (if (null? lst)
      (aunit)
      (cond [(list? (car lst)) (apair (racketlist->mupllist (car lst)) (racketlist->mupllist (cdr lst)))]
            [#t (apair (car lst) (racketlist->mupllist (cdr lst)))])))

;; Problem B

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(int? e)
         e]
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(ifgreater? e) (let([v1 (eval-under-env (ifgreater-e1 e) env)]
                             [v2 (eval-under-env (ifgreater-e2 e) env)])
                          (if (and (int? v1) (int? v2))
                              (if(> (int-num v1) (int-num v2))
                                (eval-under-env (ifgreater-e3 e) env)
                                (eval-under-env (ifgreater-e4 e) env))
                              (error "MUPL ifgreater applied to non-number")))]
        [(fst? e) (let ([v1 (eval-under-env (fst-e e) env)])
                    (if (apair? v1)
                      (eval-under-env (apair-e1 v1) env) 
                      (error "MUPL snd applied to non-pair")))]
        [(snd? e) (let ([v1 (eval-under-env (snd-e e) env)])
                    (if (apair? v1)
                      (eval-under-env (apair-e2 v1) env) 
                      (error "MUPL snd applied to non-pair")))]
        [(isaunit? e) (let ([v1 (eval-under-env (isaunit-e e) env)])
                        (if(aunit? v1)
                         (int 1)
                         (int 0)))]
        [(aunit? e) (aunit)]
        [(apair? e) (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(mlet? e) (let ([name (mlet-var e)]
                         [v (eval-under-env (mlet-e e) env)] 
                         [b (mlet-body e)])
                     (eval-under-env b (append (list (cons name v)) env)))] ; add (var name . exp) to the env list
        [(fun? e) (closure env e)]
        [(call? e) (let ([v1 (eval-under-env (call-funexp e) env)] ; function that returns a closure
                         [v2 (eval-under-env (call-actual e) env)]) ; an expression
                     (if (closure? v1)
                         (let ([environment (closure-env v1)]
                               [function (closure-fun v1)]) ; function has nameopt, formal, body
                           ;evaluate the body here with enviroment and the nameopt attached to closure
                           (if (fun-nameopt function)
                               (eval-under-env (fun-body function) (append (list (cons (fun-nameopt function) v1) (cons (fun-formal function) v2)) environment)) ; we have a name and we need to bind it to the function in the closure ie. (fun-nameopt function . v1)
                               (eval-under-env (fun-body function) (append (list (cons (fun-formal function) v2)) environment)))) ; we have #f and do not need to bind
                         (error "MUPL call applied to non-colsure first parameter")))]
        ;; "CHANGE" add more cases here
        ;; one for each type of expression
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem C

(define (ifaunit e1 e2 e3)
  (if (aunit? (eval-exp e1))
      (eval-exp e2)
      (eval-exp e3)))

(define (mlet* lstlst e2)
  (eval-under-env e2 (reverse lstlst)))

(define (ifeq e1 e2 e3 e4)
  (let ([_x (eval-exp e1)]
        [_y (eval-exp e2)])
    (if (= (int-num _x) (int-num _y))
        (eval-exp e3)
        (eval-exp e4))))

;; Problem D
(define mupl-map
  (fun "mupl-map" "f"
       (fun #f "lst"
            (ifgreater (isaunit (var "lst")) (int 0)
                                               (aunit)
                                               (let ([head (fst (var "lst"))]
                                                     [tail (snd (var "lst"))])
                                                 (apair (call (var "f") head) (call (call (var "mupl-map") (var "f")) tail)))))))

;; this binding is a bit tricky. it must return a function.
;; the first two lines should be something like this:
;;
;;   (fun "mupl-map" "f"    ;; it is  function "mupl-map" that takes a function f
;;       (fun #f "lst"      ;; and it returns an anonymous function
;;          ...
;;
;; also remember that we can only call functions with one parameter, but
;; because they are curried instead of
;;    (call funexp1 funexp2 exp3)
;; we do
;;    (call (call funexp1 funexp2) exp3)
;

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun "mupl-mapAddN" "i"
             (fun #f "lst" (call (call (var "map") (fun "addI" "x" (add (var "x") (var "i")))) (var "lst"))))))
        
