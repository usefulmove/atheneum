(load "~/repos/atheneum/atheneum.scm")

(assert (= (thread
            (iota (+ 1 8))
            (map (lambda (a) (* a a)) _)
            (apply + _))
           204))

(assert (= ((fn (* _ _)) 53)
           2809))

(assert (= (red (+ :acc (* _ _)) 0 '(3 4 5))
           50))

(assert (equal? (scan-left + '(1 2 3 4))
                '(1 3 6 10)))

(assert (not= 3 2))

(assert (not-eq? (cons 2 3) (cons 2 3)))

(assert (not-eqv? 3 3.0))

(assert (not-eqv? "eqv" "eqv"))

(assert (not-equal? '(3 1 2)
                    '(2 1 2)))

(assert (true? 0))

(assert (false? (null? '(8))))

(assert (empty? (cdr '(8))))

(assert (contains? 0 '(3 1 2 0 5 4)))

(assert (equal? (enumerate '(3 1 2))
                '((0 . 3) (1 . 1) (2 . 2))))

(assert (any?
         (lambda (element) (= 8 element))
         '(2 2 2 2 8 2 2)))

(assert (all?
         even?
         '(2 2 2 2 8 2 2)))

(assert (equal? (zip '(0 0 0)
                     '(3 1 2)
                     '(0 5 4 2))
                '((0 3 0) (0 1 5) (0 2 4))))

(assert (equal? (drop 2 '(3 1 2 0 5 4))
                '(2 0 5 4)))

(assert (equal? (drop 618 '(3 1 2 0 5 4))
                '()))

(let ((matrix '((1 2 3) (4 5 6) (7 8 9))))
  (assert (equal? matrix (transpose (transpose matrix)))))

(display "atheneum. all tests passed.") (newline)
