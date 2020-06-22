;;; Calvin Walter Heintzelman
;;; Problem Set 4
;;; recall (odd? 3) and (even? 34) functions

(define (zero-list n)
 (map - (enumerate n) (enumerate n)))

(define (initial-board n)
 (map zero-list (map (lambda (num) n) (enumerate n))))

(define (count-ones b n)
 (cond ((null? b) n)
       ((null? (first b))
	(count-ones (rest b) n))
       ((not (eq? 0 (first (first b))))
	(count-ones (cons (rest (first b)) (rest b)) (+ n 1)))
       (else
	(count-ones (cons (rest (first b)) (rest b)) n))))

(define (valid-locs b x y t result)
 (cond ((null? b) result)
       ((null? (first b))
	(valid-locs (rest b) (+ x 1) 0 t result))
       ((eq? 0 (first (first b)))
	(valid-locs (cons (rest (first b)) (rest b)) x (+ y 1) t
		    (cons (list x y t) result)))
       (else
	(valid-locs (cons (rest (first b)) (rest b)) x (+ y 1) t result))))
       
(define (moves b) ;; returns a list of (x coord, y coord, turn)
 (valid-locs b 0 0 (+ (* (modulo (count-ones b 0) 2) -2) 1) (list)))

;;adds 1 to x and y until they are both at move spot and puts the t there
(define (find-spot x y t b mov_x mov_y)
 (cond ((null? b)
	(panic "Move not valid"))
       ((null? (first b))
	(cons '() (find-spot (+ x 1) 0 t (rest b) mov_x mov_y)))
       ((and (eq? y mov_y) (eq? x mov_x))
	(cons (cons t (rest (first b))) (rest b)))
       (else
	(define continuation
	 (find-spot x (+ y 1) t (cons (rest (first b)) (rest b)) mov_x mov_y))
	(cons (cons (first (first b)) (first continuation))
	      (rest continuation)))))

(define (make-move m b) ;; should be done! :D
 (find-spot 0 0 (third m) b (first m) (second m)))

(define (check-row row)
 (cond ((eq? (first row) 1)
	(cond ((or (memq -1 row) (memq 0 row))
	       0)
	      (else
	       1)))
       ((eq? (first row) -1)
	(cond ((or (memq 1 row) (memq 0 row))
	       0)
	      (else
	       -1)))
       (else
	0)))

(define (get-col b)
 (cond ((null? b)
	'())
       (else
	(cons (first (first b)) (get-col (rest b))))))

(define (get-rest-cols b)
 (cond ((null? b)
	'())
       (else
	(cons (rest (first b)) (get-rest-cols (rest b))))))

(define (get-all-cols b)
 (cond ((null? (first b))
	'())
       (else
	(cons (get-col b) (get-all-cols (get-rest-cols b))))))

(define (check-diagnols b value)
 (cond ((null? b)
	value)
       ((eq? (first (first b)) value)
	(check-diagnols (rest (get-rest-cols b)) value))
       (else
	0)))

(define (full-b? b)
 (cond ((memq 0 (flatten b))
	#f)
       (else
	#t)))

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
	 
(define (check-win b value)
 (cond ((memq value (map check-row b))
	value)
       ((eq? value (check-diagnols b value))
	value)
       ((eq? value (check-diagnols (reverse b) value))
	value)
       ((memq value (map check-row (get-all-cols b)))
	value)
       (else
	0)))


(define (win b)
 (define win-first (check-win b 1))
 (define win-second (check-win b -1))
 (cond ((eq? win-first 1)
	(cond ((eq? win-second -1)
	       (panic "There are two winners... somehow?"))
	      (else
	       1)))
       ((eq? win-second -1)
	-1)
       (else
	0)))

(define (whose-turn b)
 (+ (* (modulo (count-ones b 0) 2) -2) 1))

;(define (m-k b k)
; (cond ((not (eq? (win b) 0))
;	'())
;       (else
;	

(define (check-loss bwin-list t)
 (cond ((null? bwin-list)
	'())
       ((or (eq? (first (first bwin-list)) 0)
	    (eq? (first (first bwin-list)) t))
	(cons (first bwin-list) (check-loss (rest bwin-list) t)))
       (else
	(check-loss (rest bwin-list) t))))

(define (check-a-win bwin-list t)
 (cond ((null? bwin-list)
	'())
       ((eq? (second (first bwin-list)) t)
	(first (first bwin-list)))
       (else
	(check-a-win (rest bwin-list) t))))

(define (end-game b t)
 (define win-states (map cons (moves b) (check-loss (map (lambda (board)
		    (cons (win board) board)) (map (lambda (m) (make-move m b))
						   (moves b))) t)))
 (define win-move (check-a-win win-states t))
 (cond ((full-b? b)
	'())
       ((null? win-states)
	(first (moves b)))
       ((null? win-move) ; make a move for everything
        (map (lambda (y) (end-game y t))
	     (map (lambda (x) (rest (rest x))) win-states)))
       (else
        win-move)))

(define (m-hat b)
 (cond ((eq? (win b) 0)
	(check-w-star b (moves b)))
       (else
	'())))

(define (check-w-star b mov)
 (cond ((null? mov)
	'())
       ((eq? (w-star b) (w-star (make-move (first mov) b)))
	(cons (first mov) (check-w-star b (rest mov))))
       (else
	(check-w-star b (rest mov)))))

(define (w-star b)
 (cond ((or (not (eq? (win b) 0)) (eq? '() (moves b)))
	(win b))
       (else
	(* (whose-turn b) (apply max (map (lambda (y) (* y (whose-turn b)))
			(map w-star (map (lambda (x)
					  (make-move x b)) (moves b)))))))))

(define (w-k k b)
 (cond ((or (not (eq? (win b) 0)) (eq? (moves b) '()))
	(win b))
       (else
	(* (whose-turn b)
	   (apply max (map (lambda (z) (* z (whose-turn b)))
			   (map (lambda (y) (w-k (- k 1) y))
				(map (lambda (x) (make-move x b))
				     (moves b)))))))))

(define (w~ k b)
 (cond ((or (not (eq? (win b) 0)) (eq? (moves b) '()))
	(win b))
       ((eq? k 0)
	(win b))
       (else
	(* (whose-turn b) (apply max (map (lambda (z) (* z (whose-turn b))) (map (lambda (y) (w~ (- k 1) y)) (map (lambda (x) (make-move x b)) (moves b)))))))))
	

(define (m~ k b)
 (cond ((not (eq? (win b) 0))
	'())
       (else
	(check-w~ b k (moves b)))))

(define (check-w~ b k mov)
 (cond ((null? mov)
	'())
       ((>= (* (whose-turn b) (w~ (- k 1) (make-move (first mov) b)))
	    (* (whose-turn b) (w~ k b)))
	(cons (first mov) (check-w~ b k (rest mov))))
       (else
	(check-w~ b k (rest mov)))))

(define (optimal-moves~ k b)
 (cond ((eq? k infinity)
        (m-hat b))
       ((>= k 0)
	(m~ k b))
       (else
	(panic "First argument must be nonnegative or infinity"))))
