(define (delete_dups A)
 (cond ((null? A)
	A)
       ((member (first A) (rest A))
	(delete_dups (rest A)))
       (else (cons (first A) (delete_dups (rest A))))))

(define (set-union A B)
 (cond ((null? A)
	(delete_dups B))
       (else (delete_dups (cons (first A) (set-union (rest A) B))))))

(define (place-n-queens-by-backtracking N)
 (map place-queen (enumerate N) (n-queens N)))

(define (attacks? qi qj delta-rows)
 (or (= qi qj) (= (abs (- qi qj)) delta-rows)))

(define (list-of n f)
 (if (= n 0) '() (cons (f) (list-of (- n 1) f))))

(define (for-each-index p l)
 (define (loop l i)
  (unless (null? l)
   (p (first l) i)
   (loop (rest l) (+ i 1))))
 (loop l 0))

(define (check-queens new-column old-columns)
 (for-each-indexed
  (lambda (old-column i)
   (when (attacks? new-column old-column (+ i 1))
    (fail)))
  old-columns))

(define (n-queens n)
 (define (loop columns)
  (if (= (length columns) n)
      columns
      (let ((column (an-integer-between 0 (- n 1))))
       (check-queens column columns)
       (loop (cons column columns)))))
 (loop '()))

(define (get-all-dom-vals dom constraint)
 (cond ((null? dom)
	'())
       (else
	(define t (find-if (lambda (xe) (constraint xe)) (list (first dom))))
	(cond ((eq? t #f)
	       (get-all-dom-vals (rest dom) constraint))
	      (else
	       (cons t (get-all-dom-vals (rest dom) constraint)))))))

;;; GFC
(define (assert-unary-constraint-gfc! constraint x)
 (define temp-dom (get-all-dom-vals (domain-variable-domain x) constraint))
 (restrict-domain! x temp-dom))

(define (get-all-dom-vals-2 bound dom constraint)
 (cond ((null? dom)
	'())
       (else
	(define t (find-if (lambda (e) (constraint bound e))
			   (list (first dom))))
	(cond ((eq? t #f)
	       (get-all-dom-vals-2 bound (rest dom) constraint))
	      (else
	       (cons t (get-all-dom-vals-2 bound (rest dom) constraint)))))))

(define (assert-binary-constraint-gfc! constraint x y)
 (for-each
  (lambda (v)
   (attach-after-demon!
    (lambda ()
     (when (bound? x)
      (define temp-y-dom
       (get-all-dom-vals-2 (binding x) (domain-variable-domain y) constraint))
      (restrict-domain! y temp-y-dom))
     (when (bound? y)
      (define temp-x-dom
       (get-all-dom-vals-2 (binding y) (domain-variable-domain x) constraint))
      (restrict-domain! x temp-x-dom)))
    v))
  (list x y)))

;;; AC
(define (assert-unary-constraint-ac! constraint x)
 (define temp-dom (get-all-dom-vals (domain-variable-domain x) constraint))
 (restrict-domain! x temp-dom))

(define (assert-binary-constraint-ac! constraint x y)
  (for-each
  (lambda (v)
   (attach-after-demon!
    (lambda ()
     (restrict-domain!
      y (reduce set-union (map (lambda (a)
				(get-all-dom-vals-2
				 a (domain-variable-domain y) constraint))
			       (domain-variable-domain x)) '()))
     (restrict-domain!
      x (reduce set-union (map (lambda (b)
				(get-all-dom-vals-2
				 b (domain-variable-domain x) constraint))
			       (domain-variable-domain y)) '())))
    v))
  (list x y)))

;;; place queens with constraint part
(define (check-attack q1 q2 delta_row)
 (not (or (= q1 q2) (= (abs (- q1 q2)) delta_row))))

(define (a-a-d! domain-variable i)
 (attach-after-demon!
  (lambda ()
   (when (bound? domain-variable) (place-queen i (binding domain-variable))))
  domain-variable))

(define (make-n-list val n)
 (cond ((eq? n 0)
	'())
       (else
	(cons val (make-n-list val (- n 1))))))

(define (do-constraints d1 d2)
 (cond ((null? d2)
	'())
       (else
	(map actual-assert (make-n-list (first d1) (length d2)) d2)
	(do-constraints (rest d1) (rest d2)))))

(define (actual-assert D1 D2)
 (assert-constraint! (lambda (x y)
		      (check-attack x y (abs (- (first D1) (first D2)))))
		     (list (second D1) (second D2))))

(define (place-n-queens-by-constraints n)
 (define domain (enumerate n))
 (define domain-variables
  (map (lambda (y) (create-domain-variable domain)) (enumerate n)))
 (define d-var-with-rows (map list (enumerate n) domain-variables))
 (do-constraints d-var-with-rows (rest d-var-with-rows))
 (map a-a-d! domain-variables (enumerate n))
 (csp-solution domain-variables first))
