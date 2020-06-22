;;;'((((a #f)) #t) (((a #t)) #f)) is an example output (note the ')

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
	(define table1 (map (lambda (x) (append x (list (list var #t)))) result))
	(define table2 (map (lambda (x) (append x (list (list var #f)))) result))	
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
