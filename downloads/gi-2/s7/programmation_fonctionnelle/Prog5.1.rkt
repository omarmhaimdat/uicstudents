;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Prog5.1) (read-case-sensitive #t) (teachpacks ((lib "valrose.rkt" "installed-teachpacks") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t write mixed-fraction #t #t none #f ((lib "valrose.rkt" "installed-teachpacks") (lib "universe.rkt" "teachpack" "2htdp")))))
(define (division a b) ; a et b ∈ N, b > 0, retourne le couple (q,r)
  (if (< a b)
      (list 0 a)
      (local [(define HR (division (- a b) b))] ; HR = Hyp. de Récurrence
        (list (+ 1 (first HR)) (second HR)))))

;(length '(6 4 #t (5 a 1) 8 "une chaîne" coucou))

(define ($length L) ; $ pour ne pas tuer la primitive length !
  (if (empty? L)
      0
      (+ 1 ($length (rest L)))))

;($length '(3 4 (5 (4 '(+ 1 3)) 2)))

(define ($member x L)
  (cond ((empty? L) #f)
        ((equal? x (first L)) #t)
        (else ($member x (rest L)))))

;($member 'rest '(3 4 rest (4 5)))

;(list-ref '(jaune rouge bleu noir) 2)

(define ($list-ref L k) ; 0 ≤ k < length(L)
  (cond ((empty? L) (error "$list-ref : Liste trop courte"))
        ((= k 0) (first L))
        (else ($list-ref (rest L) (- k 1)))))

(define ($build-list n f)
  (if (= n 0)
      empty ; la liste vide !
      (cons (f 0) ($build-list (- n 1) (lambda (i) (f (+ i 1)))))))

;($build-list 6 (lambda (x) (* 2 (/ (- x 1)(+ x 1)))))

(define ($reverse L) ; algorithme naïf en
  (if (empty? L)
      L
      (append ($reverse (rest L)) (list (first L)))))

;($reverse '(JHF 7 8 92 FSD KDS 23 KS3))

(define (croissante? L)
  (cond ((empty? L) true)
        ((empty? (rest L)) true) ; un seul élément ?
        ((<= (first L) (second L)) (croissante? (rest L)))
        (else false)))

;(croissante? '(12 23 43 45 67 78 89))

;(define L (build-list 20 (λ (i) (random 100))))

;(sort L <)

;(define L1 (build-list 10000 (λ (i) (random 100))))
;(time (void (sort L1 <)))

;(define L2 (build-list 100000 (λ (i) (random 100))))
;(time (void (sort L2 <)))

;(define L3 (build-list 1000000 (λ (i) (random 100))))
;(time (void (sort L3 <)))

(define (insertion x LT)
  (cond ((empty? LT) (list x)) ; (list x) ⬄ (cons x empty)
        ((< x (first LT)) (cons x LT))
        (else (cons (first LT) (insertion x (rest LT))))))

;(insertion 3 '(1 2 5))

(define (tri-ins L)
  (if (empty? L)
      L
      (insertion (first L) (tri-ins (rest L)))))