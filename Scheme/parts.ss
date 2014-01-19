(module parts scheme

  ;; you may include the parts database here for testing purposes, but
  ;; you should remember to remove it from your code before submission
(define PART-DB
  '((p1 200 (2 p2) (3 p3))
    (p2 8 (2 p4))
    (p3 60 (1 p5) (2 p6) (3 p7))
    (p4 2)
    (p5 2)
    (p6 6 (1 p4) (2 p5))
    (p7 6 (1 p5) (2 p9))
    (p8 90 (2 p6) (1 p3))
    (p9 2)
    (p10 90 (4 p4) (8 p9) (3 p7))
    )
  )

  ;; define any helper procedures here
  
  ;; (listSubParts lst) for SUBPARTS
  ;; input: a DB entry without <part-id> and <cost>
  ;; output: a list of immediate subparts of the part name
  (define (listSubParts lst)
	(map (lambda (pair)
		(second pair))
		lst))
		
  ;; (listPrimParts lst) for PRIM-PARTS
  ;; input: a list of subparts
  ;; output: a list of all primitive parts (deep and no duplicate)
  (define (listPrimParts lst)
    (if (empty? lst) 
        '()
        (let ([restList (listPrimParts (append (SUBPARTS (first lst)) 
                                               (rest lst)))])
          (if (IS-PRIM? (first lst))
              (if (memq (first lst) restList) 
                  restList
                  (cons (first lst) restList))
              restList))))
			  
  ;; (countP partname lst) for CAN-BUILD?
  ;; input: a part name and a list of parts
  ;; output: the number of that part in the list
  (define (countP partname lst)
    (foldr (lambda (x count)
             (if (equal? partname x)
                 (+ 1 count)
                 count))
           0
           lst))
		   
  ;; (UNION l1 l2) for ALL-PARTS
  ;; input: two lists
  ;; output: a union of the two lists
  (define (UNION l1 l2)
    (cond ((empty? l1) l2)
          ((empty? l2) l1)
          ((memq (first l1) l2)	(UNION (rest l1) l2))
          (else (UNION (rest l1) (cons (first l1) l2)))))
		  
  ;; (SumOfEachSubPart lst) for COST-OF-PARTS
  ;; input: a DB entry without <part-id> and <cost>
  ;; output: a list of sums for each subpart
  (define (SumOfEachSubPart lst)
	(map (lambda (pair)
		  (* (first pair) (COST (second pair))))
		lst))
		   
  ;; end of helper procedures

  ;; (COST partname)			
  (define (COST partname)			
    (foldr (lambda (x r)
             (if (equal? partname (first x)) (second x) r))
           0
           PART-DB))

  ;; (SUBPARTS partname) 
  (define (SUBPARTS partname)
    (foldr (lambda (x r)
             (if (equal? partname (first x)) (listSubParts (drop x 2)) r))
           '()
           PART-DB))

  ;; (IS-PRIM? partname)
  (define (IS-PRIM? partname)
    (foldr (lambda (x r)
             (if (equal? partname (first x)) 
                 (if (empty? (listSubParts (drop x 2))) true false) 
                 r))
           false
           PART-DB))

  ;; (NOT-SUBPART? partname)
  (define (NOT-SUBPART? partname)
      (foldr (lambda (x r)
             (if (memq partname (listSubParts (drop x 2)))
                 false
                 r))
           true
           PART-DB))

  ;; (AFFORDLIST val)
  (define (AFFORDLIST val)
    (foldr (lambda (x r)
             (if (>= val (second x)) 
				(cons (first x) r)  
				r))
           '()
           PART-DB))
		   
  ;; (PRIM-PARTS partname)
  (define (PRIM-PARTS partname)
	(listPrimParts (SUBPARTS partname)))
	
	(define (buildTest? l1 l2)
		(cond ((empty? l1) true)
			  ((< (countP (second (first l1)) l2) (first (first l1))) false)
			  (else (buildTest? (rest l1) l2))))

  ;; (CAN-BUILD? partname partlist)
  (define (CAN-BUILD? partname lst)
    (foldr (lambda (x r)
             (if (equal? partname (first x)) (buildTest? (drop x 2) lst) r))
           false
           PART-DB))

  ;; (BUILD-LIST partlist)
  (define (BUILD-LIST lst)
    (foldr (lambda (x r)
             (if (and (not (IS-PRIM? (first x)))
                      (CAN-BUILD? (first x) lst))
                 (cons (first x) r)
                 r))
           '()
           PART-DB))  

  ;; (ALL-COST partlist)
  (define (ALL-COST lst)
	(apply +(map COST lst)))

  ;; (ALL-PARTS partlist)
  (define (ALL-PARTS lst)
	(foldr (lambda (x r)
			(UNION (SUBPARTS x) r))
		'()
		lst))

  ;; (COST-OF-PARTS partname)
  (define (COST-OF-PARTS partname)
    (foldr (lambda (x r)
             (if (equal? partname (first x)) 
				(apply + (SumOfEachSubPart (drop x 2)))
				r))
           0
           PART-DB))

  (provide COST SUBPARTS IS-PRIM? NOT-SUBPART? AFFORDLIST PRIM-PARTS CAN-BUILD? BUILD-LIST ALL-COST ALL-PARTS COST-OF-PARTS)
)
