(define (single_pass l compare)
  (if (null? (cdr l))
    l
    (let ((first (car l)) (second (cadr l)))
      (if (compare first second)
        (cons first (single_pass (cdr l) compare))
        (cons second (single_pass (cons first (cddr l)) compare))
      )
    )
  )
)

(define (forward_pass l)
  (single_pass l <=)
)

(define (reverse_pass l)
  (reverse (single_pass (reverse l) >))
)

(define (shaker_sort_impl l counter)
  (if (null? counter)
    l
    (shaker_sort_impl (reverse_pass (forward_pass l)) (cdr counter))
  )
)

(define (shaker_sort l)
  (shaker_sort_impl l l)
)


(shaker_sort `())
(shaker_sort `(42))
(shaker_sort `(1 2))
(shaker_sort `(2 1))
(shaker_sort `(1 2 3 4 5 6 7 8 9 10))
(shaker_sort `(10 9 8 7 6 5 4 3 2 1))
(shaker_sort `(5 2 3 7 1 6 10 4 9 8))
