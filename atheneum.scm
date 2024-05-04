;; macro support

(define (create-lambda sexp)
  `(lambda (_arg_)
     ,(replace-underscore sexp '_arg_)))


(define (replace-underscore sexp value)
  (cond ((eq? sexp '_) value)
        ((not (pair? sexp)) sexp)
        (else (cons (replace-underscore (car sexp) value)
                    (replace-underscore (cdr sexp) value)))))


;; thread
(define-syntax thread
  (syntax-rules ()
    ((_ seed) seed)
    ((_ seed form more ...)
     (thread ((eval (create-lambda 'form)) seed) more ...))))


;; fn (function)
(define-syntax fn
  (syntax-rules ()
    ((_ form) (eval (create-lambda 'form)))))


;; red (reduce)
(define-syntax red
  (syntax-rules ()
    ((_ form seed lst)
     (fold-left (eval `(lambda (:acc _) form)) seed lst))))


;; scan
(define (scan-left f lst)
  (let ((last (lambda (lst)
                (car (reverse lst)))))
    (fold-left
     (lambda (acc a) 
       (if (null? acc) (append acc (list a))
           (append acc (list (f a (last acc))))))
     '()
     lst)))


;; debug
(define-syntax debug
  (syntax-rules ()
    ((_ sexp) (let ((value sexp))
                (display value)
                (newline)
                value))))


;; not=
(define-syntax not=
  (syntax-rules ()
    ((_ sexp more ...) (not (= sexp more ...)))))


;; not-equal?
(define-syntax not-equal?
  (syntax-rules ()
    ((_ sexp more ...) (not (equal? sexp more ...)))))


;; not-eq?
(define-syntax not-eq?
  (syntax-rules ()
    ((_ sexp more ...) (not (eq? sexp more ...)))))


;; true?
(define-syntax true?
  (syntax-rules ()
    ((_ sexp) (not (not sexp)))))


;; false?
(define-syntax false?
  (syntax-rules ()
    ((_ sexp) (not sexp))))


;; empty?
(define-syntax empty?
  (syntax-rules ()
    ((_ sexp) (null? sexp))))


;; contains?
(define-syntax contains?
  (syntax-rules ()
    ((_ element lst) (not (not (member element lst))))))


;; all?



;; any?



;; flatten



;; compose



;; pipe



;; init



;; last



;; tail



;; head



;; first



;; take



;; drop



;; enumerate
(define (enumerate lst . params)
  (let ((index (if (not (null? params)) (car params) 0)))
    (cond ((null? lst) '())
          (else (cons (cons index
                            (car lst))
                      (enumerate (cdr lst) (+ 1 index)))))))



;; zip



;; zip-with



;; memoize



;; count-elements



;; remove-duplicates



;; when-false
