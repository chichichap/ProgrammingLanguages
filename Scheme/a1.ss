(module a1 scheme

  ;; (swap13 lst)
  (define swap13 
    (lambda (lst)
      (if (< (length lst) 3) 
          lst 
          (append (list (third lst)) 
                  (list (second lst)) 
                  (list (first lst)) 
                  (drop lst 3)))
      )
    )

  ;; (deep-swap13 lst)
  (define deep-swap13 
    (lambda (lst)
      (cond ((empty? lst) lst)
            ((not (list? lst)) lst)
            ((< (length lst) 3) (append (list (deep-swap13 (first lst))) 
                                        (deep-swap13 (rest lst))))
            ((> (length lst) 5) (append (list (deep-swap13 (third lst))) 
                                        (list (deep-swap13 (second lst))) 
                                        (list (deep-swap13 (first lst))) 
                                        (deep-swap13 (swap13 ((drop lst 3))))))
            (else (append (list (deep-swap13 (third lst))) 
                          (list (deep-swap13 (second lst))) 
                          (list (deep-swap13 (first lst))) 
                          (deep-swap13 (drop lst 3)))))
      )
    )


  ;; (mem? atom lst) 
  (define mem? 
    (lambda (atom lst)
      (cond ((empty? lst) false)
            ((equal? atom (first lst)) true)
            (else (mem? atom (rest lst))))
      )
    )

  ;; (deep-mem? atom lst) 
  (define deep-mem? 
    (lambda (atom lst)
      (cond ((empty? lst) false)
            ((list? (first lst)) (or (deep-mem? atom (first lst)) 
                                     (deep-mem? atom (rest lst))))
            (else (or (equal? atom (first lst)) 
                      (deep-mem? atom (rest lst)))))
      )
    )

  ;; (smallest-prime? n divider)
  ;; returns the smallest prime factor of a given integer n > 1
  ;; the divider is for the recursion to keep increasing the divider
  (define smallest-prime
    (lambda (n divider)
      (if (equal? (modulo n divider) 0)
          divider
          (smallest-prime n (+ divider 1)))
      )
    )  
  
  ;; (prime-factors n)
  (define prime-factors 
    (lambda (n)
      (if (equal? n 1)
          '()
          (append (list (smallest-prime n 2)) 
                  (prime-factors (/ n (smallest-prime n 2)))))
      )
    )
  
  ;; (count-duplicates lst)
  ;; returns the number of prime factor duplicates in a given list
  ;; only works with non-decreasing lists
  (define count-duplicates
    (lambda (lst)
      (cond ((< (length lst) 2) 0)
            ((equal? (first lst) (second lst)) (+ 1 (count-duplicates (rest lst))))
            (else (count-duplicates (rest lst))))
      )
    )

  ;; (prime-factors n)
  (define count-distinct-factors
    (lambda (n)
      (- (length (prime-factors n)) 
         (count-duplicates (prime-factors n)))
      )
    )

  (provide swap13 deep-swap13 mem? deep-mem? prime-factors count-distinct-factors)
)
