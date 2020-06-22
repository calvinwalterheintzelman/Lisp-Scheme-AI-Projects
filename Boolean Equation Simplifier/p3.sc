;;; Calvin Walter Heintzelman
;;; ECE473
;;; cheintze
;;; 3/18/2019
;;; Problem set 3

;;; EVERYTHING BELOW IS FROM PROBLEM SET 2

;;; '((((a #f)) #t) (((a #t)) #f)) is an example output (note the ')

;;; phi can be the following:
;;; a boolean
;;; a variable
;;; (not phi0)
;;; (and phi1 phi2 ... phin)
;;; (or phi1 phi2 ... phin)

;;; By Calvin Walter Heintzelman

(define (and-f a b)
 (and a b))

(define (or-f a b)
 (or a b))

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

(define (flatten lists)
 (cond ((null? lists)
	lists)
       ((null? (first lists))
	(flatten (rest lists)))
       ((list? (first lists))
	(flatten (cons (rest (first lists))
		       (cons (first (first lists)) (rest lists)))))
       (else
	(cons (first lists) (flatten (rest lists))))))

(define (parameters phi)
 (cond ((null? phi)
	'())
       ((boolean? phi)
	'())
       ((symbol? phi)
	(list phi))
       ((list? phi)
	(cond ((and (eq? (first phi) 'not) (eq? (length phi) 2))
	       (parameters (second phi)))
	      ((eq? (first phi) 'and)
	       (delete_dups (flatten (map parameters (rest phi)))))
	      ((eq? (first phi) 'or)
	       (delete_dups (flatten (map parameters (rest phi)))))
	      (else
	       (panic "Invalid expression!"))))
       (else
	(panic "Invalid expression!"))))
	
(define (bindings p-list result)
 (cond ((null? p-list)
	result)
       ((null? result)
	(bindings (rest p-list) (list (list (list (first p-list) #t))
			(list (list (first p-list) #f)))))
       (else
	(define var (first p-list))
	(define table1 (map (lambda (x)
			     (append x (list (list var #t)))) result))
	(define table2 (map (lambda (x)
			     (append x (list (list var #f)))) result))	
	(bindings (rest p-list) (append table1 table2)))))

(define (find-mapping x mapping)
 (cond ((boolean? x)
	x)
       ((eq? (first (first mapping)) x)
	(second (first mapping)))
       (else
	(find-mapping x (rest mapping)))))

(define (find-truth phi mapping)
 (cond ((boolean? phi)
	phi)
       ((symbol? phi)
	(find-mapping phi mapping))
       ((eq? (first phi) 'not)
	(not (find-truth (second phi) mapping)))
       ((eq? (first phi) 'and)
	(reduce and-f (map (lambda (x) (find-truth x mapping)) (rest phi)) #t))
       ((eq? (first phi) 'or)
	(reduce or-f (map (lambda (x) (find-truth x mapping)) (rest phi)) #f))))

(define (create-table b-list result phi)
 (cond ((null? b-list)
	result)
       (else
	(define mapping (first b-list))
	(define round-result (list mapping (find-truth phi mapping)))
	(define new-result (append result (list round-result)))
	(create-table (rest b-list) new-result phi))))

(define (truth-table phi)
 (define p-list (parameters phi))
 (define b-list (bindings p-list (list)))
 (create-table b-list (list) phi))

;;; EVERYTHING ABOVE IS FROM PROBLEM SET 2

(define (same-vars? phi1 phi2)
 (cond ((null? phi2)
	#t)
       ((member (first phi2) phi1)
	(same-vars? phi1 (rest phi2)))
       (else
	#f)))

(define (truth-value phi1 phi2 b-list)
 (cond ((null? b-list)
	#t)
       ((equal?
	 (find-truth phi1 (first b-list)) (find-truth phi2 (first b-list)))
	(truth-value phi1 phi2 (rest b-list)))
       (else
	#f)))

(define (truth-tables-match? phi1 phi2)
 (cond ((same-vars? (parameters phi1) (parameters phi2))
	(define b-list (bindings (parameters phi1) (list)))
	(truth-value phi1 phi2 b-list))
       (else
	#f)))

(define (reverse-list l r) ; r should be passed in as (list)
 (cond ((null? l)
	r)
       (else
	(reverse-list (rest l) (cons (first l) r)))))

(define (check-repeat phi symbol)
 (cond ((null? phi)
	phi)
       ((and (list? (first phi)) (equal? (first (first phi)) symbol))
	(append (rest (check-repeat (first phi) symbol))
		(check-repeat (rest phi) symbol)))
       (else
	(cons (first phi) (check-repeat (rest phi) symbol)))))

(define (check-clone phi)
 (cond ((null? phi)
	phi)
       ((member (first phi) (rest phi))
	(cons (first phi) (check-clone (remove (first phi) (rest phi)))))
       (else
	(cons (first phi) (check-clone (rest phi))))))

(define (check-double-not phi)
 (cond ((null? phi)
	#f)
       ((member (cons 'not (list (first phi))) (rest phi))
	#t)
       ((and (list? (first phi))
	     (equal? 'not (first (first phi)))
	     (member (second (first phi)) (rest phi)))
	#t)
       (else
	(check-double-not (rest phi)))))

(define (boolean-simplify phi)
 (cond ((null? phi)
	phi)
       ((boolean? phi)
	phi)
       ((symbol? phi)
	phi)
       ((list? phi)
	(cond ((and (equal? (first phi) 'not) (equal? (length phi) 2))
	       (cond ((equal? (second phi) #t)
		      #f)
		     ((equal? (second phi) #f)
		      #t)
		     ((and (list? (second phi))
			   (equal? (first (second phi)) 'not))
		      (boolean-simplify (second (second phi))))
		     (else
		      (cons 'not (list (boolean-simplify (second phi)))))))
	      ((equal? (first phi) 'and)
	       (define and-phis (cons 'and (check-repeat (rest phi) 'and)))
	       (define and-phis
		(cons 'and (map boolean-simplify (rest and-phis))))
	       (define and-phis (cons 'and (check-clone (rest and-phis))))
	       (cond ((equal? (length and-phis) 1)
		      #t)
		     ((equal? (length and-phis) 2)
		      (boolean-simplify (second and-phis)))
		     ((member #f and-phis)
		      #f)
		     ((member #t and-phis)
		      (boolean-simplify (remove #t and-phis)))
		     ((check-double-not and-phis)
		      #f)
		     (else
		      and-phis)))
	      ((equal? (first phi) 'or)
	       (define or-phis (cons 'or (check-repeat (rest phi) 'or)))
	       (define or-phis
		(cons 'or (map boolean-simplify (rest or-phis))))
	       (define or-phis (cons 'or (check-clone (rest or-phis))))
	       (cond ((equal? (length or-phis) 1)
		      #f)
		     ((equal? (length or-phis) 2)
		      (boolean-simplify (second or-phis)))
		     ((member #t or-phis)
		      #t)
		     ((member #f or-phis)
		      (boolean-simplify (remove #f or-phis)))
		     ((check-double-not or-phis)
		      #t)
		     (else
		      or-phis)))
	      (else
	       (panic "Invalid expression!"))))
	(else
	(panic "Invalid expression!"))))
