;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Prog4.1) (read-case-sensitive #t) (teachpacks ((lib "valrose.rkt" "installed-teachpacks") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t write mixed-fraction #t #t none #f ((lib "valrose.rkt" "installed-teachpacks") (lib "universe.rkt" "teachpack" "2htdp")))))
(define (fac n)
  (if (= n 0) 1
      (* n (fac (- n 1)))))

(define (nbchiffres n)
  (if (< n 10)
      1
      (+ 1 (nbchiffres (quotient n 10)))))

(define (fac2 n)
  (cond ((< n 0) (error 'fac2 "On attendait un entier positif : " n))
        ((= n 0) 1)
        (else (* (fac (- n 1)) n))))

(define (fac3 n)
  (local [(define (aux n)
            (if (= n 0)
                1
                (* (aux (- n 1)) n)))]
    (if (>= n 0)
        (aux n)
        (error 'fac3 "On attendait un entier positif : " n))))

(define ($expt x n) ; x complexe, n entier ≥ 0
  (if (= n 0)
      1
      (* x ($expt x (- n 1)))))

(define ($expt1 x n) ; x complexe, n entier ≥ 0
  (cond ((= n 0) 1)
        ((even? n) (sqr ($expt1 x (quotient n 2))))
        (else (* x (sqr ($expt1 x (quotient n 2)))))))

(define (serrements-de-main n)
  (if (= n 0) 0
      (+ (- n 1) (serrements-de-main (- n 1)))))

(define (oursin n)
  (if (= n 0)
      (circle 0 'solid "white") ; image vide  !    
      (underlay                 ; on pose ....
       (oursin (- n 1))         ;  ..... sur un oursin de n - 1 épines
       (rotate (random 360)     ;  ..... une nouvelle épine
               (line (random 100) (random 100) "blue")))))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (pgcd a b)
  (if (= b 0) a
      (pgcd b (modulo a b))))

;(time ($expt 999 8765))  ; près de 120 ms
;(time ($expt1 999 8765)) ; 0 ms 
;(time (fib 30)) ; oups

(define (nb-chemins-rect L H) ; monde rectangulaire
  (if (or (= L 0) (= H 0))
      1
      (+ (nb-chemins-rect (- L 1) H) ; en passant par C
         (nb-chemins-rect L (- H 1))))) ; en passant par D

(define (nb-chemins N) ; le carré comme cas particulier
  (nb-chemins-rect N N))

(define (carré size)
  (rectangle size size 'outline "blue")
  )

(define (carrés-emboîtés n size incr) ; récurrence sur n ≥ 1
  (if (= n 1)
      (carré size)
      (underlay (carré size)
                (carrés-emboîtés (- n 1) (+ size incr) incr))))


(define (intégrale f a b)
    (if (>= a b) 
        0
        (+ (* #i0.001 (f a)) (intégrale f (+ #i0.001 a) b))
        ))

;(intégrale (lambda(x) (+ (sqr x) 2)) 2 4)