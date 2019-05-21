; stdlib.scm

(define (not x) (if x #f #t))
(define (null? obj) (if (eqv? obj '()) #t #f))

; defining a list taking variable args and returning them
(define (list . objs) objs)

; identity functions are quite useful when a function expects another function
; as argument returning a certain type of variable
; higher order functions lets us utilize this power
(define (id obj) obj)

(define (flip func) (lambda (arg1 arg2) (func arg2 arg1)))

(define (curry func arg1)
    (lamda (arg)
        (apply func
            (cons arg1 (list arg)))))

(define (compose f g)
    (lambda (args)
        (f (apply g args))))

(define zero? (curry = 0))
(define positive? (curry > 0))
(define negative? (curry < 0))
(define (odd? num) (= (mod num 2) 1))
(define (even? num) (= (mod num 2) 0))

; (foldr + 5 '(1 2 3 4)) = (1 + (2 + 3 + (4 + 5)))
; In Haskell, foldr :: (b -> a -> a) -> a -> [b] -> a
(define (foldr func end list)
    (if (null? list)
        end
        (func (car list) (foldr func end (cdr list)))))

; foldl happens to be tail-recursive. it need not recurse all the way to end of list
; to build up computation. Hence it is runs more efficiently than foldr
; In Haskell, foldl :: (a -> b -> a) -> a -> [b] -> a
(define (foldl func acc list)
    (if (null? list)
        acc
        (foldl func (func acc (car list)) (cdr list))))

(define fold foldl)
(define reduce foldr)

; Given a unary function, an initial value, and a unary predicate, it continues applying the function 
; to the last value until the predicate is true, building up a list as it goes along.
(define (unfold func init pred)
    (if (pred init)
        (cons init '())
        (cons init (unfold func (func init) pred))))

; Some gyan!!!
; In academic functional programming literature, folds are often called catamorphisms, unfolds are often called anamorphisms,
; and the combinations of the two are often called 
; hylomorphisms. They're interesting because any for-each loop can be represented as a catamorphism. To convert from a loop to a foldl, 
; package up all mutable variables in the loop 
; into a data structure (records work well for this, but you can also use an algebraic data type or a list). 
; The initial state becomes the accumulator; the loop body becomes a function with the loop variables as its first argument and the iteration variable 
; as its second and the list becomes, well, the list. The result of the fold function is the new state of all 
; the mutable variables.

; Similarly, every for-loop (without early exits) can be represented as a hylomorphism. The initialization, termination, and step conditions of a for-loop define an anamorphism that 
; builds up a list of values for the iteration variable to take. Then, you can treat that as a for-each loop and use a catamorphism to break it down into whatever state you wish to 
; modify.

; dot allows for variable number of arguments on the right
(define (sum . list) (fold + 0 list)) ; can be used as (sum 1 2 3 4)
(define (product . list) (fold * 1 list))
(define (and . list) (fold && #t list))
(define (or . list) (fold || #f list))

(define (max first . rest)
    (fold (lambda (old new)
            (if (> old new)
                old
                new))
          first
          rest))
         
(define (min first . rest)
    (fold (lambda (old new)
            (if (< old new)
                old
                new))
          first
          rest))

(define (length list)
    (fold (lamda (x y) (+ x 1))
          0
          list))

; you think I'm funny, wait till you see my code

(define (reverse list)
    (fold (flip cons)
          '()
          list))

(define (map func list)
    (foldr (lambda (x y)
             (cons (func x) y))
           '()
           list))

(define (filter pred list)
    (foldr (lambda (x y)
             (if (pred x) (cons x y) y))
           '()
           list))
