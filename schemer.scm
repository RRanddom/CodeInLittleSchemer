;;                                                    Learning Scheme with The Little Schemer!
  
#lang scheme 
  
  ;; try to build lists with cons
  ;; for example (a b (c))
  (define list-l
    (cons "a" (cons "b" (cons (cons "c" '()) '()))))
  ;; define lat (a b c d e f)
   (define lat-l
      (cons 'a (cons 'b (cons 'c (cons 'd (cons 'e (cons 'f '())))))))
  ;;define lat?
  ;;what is "lat?"  first,lat is a list of atom, so you know what lat? is.
  ;;lat? takes a list;

   (define lat?                                                 ;; 
     (lambda (l)                                                ;; lats is a procedure
      (cond                                                     ;; if-then-else
         ((null? l) #t)                                         ;; l is an empty list--return true
         ((atom? (car l)) (lat? (cdr l)))                       ;; (car l) is atom?--exam (cdr l)
         (else #f))))                                           ;; (else l is not lat)
  ;;since we do not have atom?
    (define atom?
       (lambda (x)
        (and (not (pair? x)) (not (null? x)))))
  ;;define member?
  ;;member takes two arguments.the first one is atom, the next one is a lat--you know lat!
  ;; if the atom is a member of lat--true,else--false

  (define member?                                              ;;
   (lambda (a lat)                                             ;; member is a procedure with two variables
    (cond                                                      ;; if-then-else
      ((null? lat) #f)                                         ;; lat is an empty list,so it is impossible that lat contains a
      (else (or (eq? (car lat) a)                              ;; if (car lat) eqs a,lat must contain a,
              (member? a (cdr lat)))))))                       ;; if a is a member of list (cdr lat) ,a must be a member of lat

   ;;define rember
   ;;takes two arguments.--an atom and a lat.
   ;;remove the first atom in the lat
   ;;The function rember checked each atom of the lat , one at a time, to see if it was the
   ;;same as the atom and. If the car was not;the same as the atom , we saved it to be consed
   ;; to the final value later. When rember found the atom and, it dropped it,and consed the
   ;;previous atoms back onto the rest of the lat




  ;;  simplify
    (define rember
      (lambda (a lat)
        (cond 
         ((null? lat)  '())
         ((eq? (car lat) a) (cdr lat))
          (else (cons (car lat)
                  (rember a (cdr lat)))))))

  ;;define firsts
  ;;The function firsts takes one argument--a list,which is either a null list
  ;;or contains only non-empty lists.It builds another list composed of first  
  ;;S-expression of each internal list.
     
    (define firsts
     (lambda (l)  
      (cond
       ((null? l) (quote()))
       (else (cons (car (car l))
                   (firsts (cdr l))))))) 

  ;;a list ((a b) (c d) (e f))
     (define list-m
        (cons (cons "a" (cons "b" '())) (cons (cons "c" (cons "d" '())) (cons (cons "e" (cons "f" '())) '()))))
     
  ;;define insertR
  ;;It takes three arguments: the atom new and old ,and a lat .
  ;;The function insertR builds a lat with new inserted to the
  ;;right of the first occurrence of old . 
   
      (define insertR
         (lambda (new old lat)
            (cond
              ((null? lat) (quote()))
              ((eq? old (car lat)) (cons (car lat) (cons new (cdr lat))))
              (else (cons (car lat) (insertR new old (cdr lat))))))) 
      
   ;;define subst
   ;;(subst new old lat)replace the first occurance of old in the lat
   ;;with new 
      (define subst
        (lambda (new old lat)
          (cond 
            ((null? lat) (quote()))
            ((eq? (car lat) old)
                (cons new (cdr lat)))
            (else (cons (car lat)
                      (subst new old
                              (cdr lat)))))))
      
    ;;define subst2  
    ;;(subst2 new o1 o2 lat) replaces either the first occurrence of o1 or
    ;;the first occurrence of o2 by new 
       (define subst2
         (lambda (new o1 o2 lat)
            (cond
              ((null? lat) (quote()))
              ((or (eq? (car lat) o1)
                   (eq? (car lat) o2))
                      (cons new (cdr lat)))
              (else (cons (car lat)
                         (subst2 new o1 o2 (cdr lat)))))))
       
    ;;define multirember
    ;;(multirember a lat) removes all the atom eq? a in the lat
       (define multirember
          (lambda (a lat)
             (cond
                ((null? lat) (quote ()))
                ((eq? (car lat) a) (multirember a (cdr lat)))
                (else (cons (car lat) 
                           (multirember a (cdr lat)))))))
       
   ;;define multiinsertR
        (define multiinsertR
           (lambda (new old lat)
             (cond
                ((null? lat) (quote()))
                ((eq? (car lat) old) 
                         (cons old 
                          (cons new 
                           (multiinsertR new old (cdr lat)))))
                (else (cons (car lat)
                       (multiinsertR new old (cdr lat)))))))
        
   ;;define multisubst
   ;;replaces old with new  (multisubst new old lat)
          (define multisubst
            (lambda (new old lat)
               (cond 
                  ((null? lat) (quote()))
                  ((eq? (car lat) old)
                      (cons new
                            (multisubst new old (cdr lat))))
                  (else (cons (car lat)
                            (multisubst new old (cdr lat)))))))   
          
   ;;number games;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;define o+ with add1 and sub1
          (define o+
             (lambda (m n)
               (cond 
                 ((zero? n) 
                      m)
                 (else (o+ (add1 m) (sub1 n))))))
          
   ;;define o- with sub1 and add1
            (define o-
              (lambda (m n)
                 (cond
                   ((zero? n)
                      m)
                    (else (o- (sub1 m) (sub1 n))))))
            
   ;;define addtup
            (define addtup
               (lambda (tup)
                  (cond
                    ((null? tup) 0)
                    (else (o+ (car tup) (addtup (cdr tup)))))))
            
   ;;define o*  (o* a b) returns (* a b)
             (define o*
                (lambda (m n)
                  (cond 
                     ((zero? n)
                         0)
                     (else (o+ m (o* m (sub1 n)))))))
   ;;define tup+
              (define tup+
                (lambda(tup1 tup2)
                  (cond 
                    ((and (null? tup1) (null? tup2))
                     (quote()))
                    (else
                      (cons (o+ (car tup1) (car tup2))
                          (tup+
                            (cdr tup1) (cdr tup2)))))))
              
   ;;a better tup+ which can work for any two tups
               (define tup++
                  (lambda(tup1 tup2)
                    (cond
                       ((null? tup1) tup2)
                       ((null? tup2) tup1)
                       (else
                        (cons (o+ (car tup1) (car tup2))
                              (tup++
                                 (cdr tup1) (cdr tup2)))))))
   ;;define o> (o> 120 1) returns #t
   ;;(o> 10 100) returns #f (o> 100 100) returns #f
               (define o>
                 (lambda (m n)
                   (cond 
                     ((zero? m) #f)
                     ((zero? n) #t)
                     (else
                        (o> (sub1 m) (sub1 n))))))
               
   ;;define o< 
             (define o<
               (lambda (m n)
                 (cond 
                    ((zero? n) #f)
                    ((zero? m) #t)
                    (else 
                       (o< (sub1 m) (sub1 n))))))
             
   ;;define o=
             (define o=
               (lambda (m n)
                 (cond
                   ((or (o> m n) (o< m n)) #f)
                   (else #t))))
    
   ;;define oexpt
             (define oexpt
               (lambda (m n)
                 (cond 
                   ((zero? n) 1)
                   (else
                     (o* m (oexpt m (sub1 n)))))))
             
   ;;define quotient/division (o/ m n) returns m/n
             (define o/
               (lambda (m n)
                 (cond
                   ((o< m n) 0)
                   (else (add1 (o/ (o- m n) n))))))
             
    ;;length lat .caculate the length of a lat
              (define length
                (lambda (lat)
                  (cond 
                     ((null? lat) 0)
                     (else
                        (add1 (length (cdr lat)))))))
              
   ;;define pick   (pick n lat) returns the nth atom in the lat
                (define pick
                  (lambda (n lat)
                    (cond
                      ((zero? n) (quote "No answer"))
                      ((zero? (sub1 n)) (car lat))
                      (else (pick (sub1 n) (cdr lat))))))
                
   ;;define rempick for example (rempick 3 (hotdogs with hot mustard)) returns (hotdogs with mustard) 
                (define rempick
                  (lambda (n lat)
                    (cond 
                      ((zero? n) lat)
                      ((zero? (sub1 n)) (cdr lat))
                      (else (cons (car lat) 
                               (rempick (sub1 n) (cdr lat)))))))
                
   ;;define number?
   ;;sorry! numbers? is a primitive procedure which cannot be "defined" by other procedure
       
                
   ;define no-nums  (no-nums lat) for example (no-nums (5 pears 6 prunes 9 dates))
   ;;                                         returns (pears prunes dates)
                
                (define no-nums
                  (lambda (lat)
                   (cond
                    ((null? lat) '())
                    ((number? (car lat))
                              (no-nums (cdr lat)))
                    (else (cons (car lat)
                             (no-nums (cdr lat)))))))
                
   ;;define all-nums
                 (define all-nums
                   (lambda (lat)
                     (cond
                       ((null? lat) '())
                       ((number? (car lat))(cons (car lat)
                                    (all-nums (cdr lat))))
                       (else (all-nums (cdr lat))))))
                 
   ;;define eqan?  (eqan? a b) if a,b are nums and a=b,or if a,b are atoms and a eq b,returns #t.else returns #f
                 
                 (define eqan?
                   (lambda (a b)
                     (cond
                        ((and (number? a) (number? b))
                             (o= a b))
                        ((or (number? a) (number? b))
                             #f)
                        (else (eq? a b)))))
                 
   ;;define occur   (occur a lat) counts the number of times that a occurs in the lat
                 (define occur
                   (lambda (a lat)
                     (cond
                        ((null? lat) 0)
                        ((eq? (car lat) a) 
                            (add1 (occur a (cdr lat))))
                        (else (occur a (cdr lat))))))
                 
   ;;define one?
                  (define one?
                     (lambda(n)
                       (o= n 1)))
                  
   ;;rewrite rempick with one?     
                (define rempick2 
                  (lambda (n lat)
                    (cond
                      ((zero? n) (quote "No Answer"))
                      ((one? n) (cdr lat))
                      (else (cons (car lat)
                               (rempick2 (sub1 n)
                                        (cdr lat)))))))
                
   ;;define rember* which pronounced "rember-star"
   ;;(rember* a l) a=cup l=((coffee) cup ((tea) cup) (and (hick)) cup)
   ;;returns ((coffee) ((tea)) (and (hick)))
                (define rember*
                  (lambda (a l)
                    (cond
                      ((null? l) '())
                      ((eq? (car l) a) (rember* a (cdr l)))
                      ((atom? (car l))
                          (cons (car l) (rember* a (cdr l))))
                      (else (cons (rember* a (car l))
                              (rember* a(cdr l))))))) ;;a bit ugly
                
   ;;define insertR*
   ;;the procedure asks three questions
   ;;the list is null? || (car list) is an atom?|| else
                 (define insertR*
                   (lambda (new old l)
                     (cond
                       ((null? l) '())
                       ((atom? (car l))
                         (cond
                            ((eq? (car l) old)
                             (cons old (cons new
                                          (insertR* new old (cdr l)))))
                            (else (cons (car l)
                                      (insertR* new old (cdr l))))))
                       (else (cons (insertR* new old (cdr l))
                               (insertR* new old (cdr l)))))))
   ;;define occur* (occur* a l) a is atom and l is list
   ;;return the number of times that a occurs in the list!
                 (define occur*
                   (lambda (a l)
                     (cond 
                        ((null? l) 0)
                        ((atom? (car l))
                           (cond
                              ((eq? (car l) a)
                                   (add1 (occur* a (cdr l))))
                              (else (occur* a (cdr l)))))
                        (else (o+ (occur* a (car l))
                                  (occur* a (cdr l)))))))
                          
                
   ;;define subst* (subst* new old)
   ;;replace all the old with new in the list
                 (define subst*
                   (lambda (new old l)
                     (cond 
                       ((null? l) '())
                       ((atom? (car l))
                           (cond
                              ((eq? (car l) old) (cons new
                                                   (subst* new old (cdr l))))
                              (else (cons (car l)
                                      (subst* new old (cdr l))))))
                       (else (cons (subst* new old (car l))
                               (subst* new old (cdr l)))))))
                     
                
   ;;define insertL* (insertL* new old l)
   ;;very similiar with insertR*                
                 (define insertL*
                   (lambda (new old l)
                     (cond
                       ((null? l) '())
                       ((atom? (car l))
                         (cond
                            ((eq? (car l) old) (cons new
                                                 (cons old (insertL* new old (cdr l)))))
                            (else (cons (car l) (insertL* new old (cdr l))))))
                       (else (cons (insertL* new old (car l)) 
                               (insertL* new old (cdr l)))))))
                 
             
                
   ;;define member*
   ;;(member* a l) returns true or false
                  (define member*
                    (lambda (a l)
                      (cond
                        ((null? l) #f)
                        ((atom? (car l))
                           (or (eq? (car l) a)
                              (member* a (cdr l))))
                        (else (or (member* a (car l))
                                  (member* a (cdr l)))))))
                      
                
   ;;define leftmost
   ;;The function leftmost finds the leftmost atom in a non-empty list of S-expressions
   ;;that does not contain the empty list.
                (define leftmost
                  (lambda (l)
                    (cond
                      ((null? l) (quote "NO Answer"))
                      ((atom? (car l)) (car l))
                      (else (leftmost (car l))))))
                   
   ;;important tips (and ..) (or..) both can be defined as functions in terms of
   ;;(cond..) (and alpha beta)=(cond (alpha beta) (else #f))
   ;;         (or alpha beta)=(cond (alpha #t) (else beta))
    
                
   ;;define eqlist?
   ;;(eqlist? l1 l2) returns #t iff l1==l2 ;5 questions total
                (define eqlist?
                  (lambda (l1 l2)
                    (cond
                      ((and (null? l1) (null? l2)) #t)
                      ((or (null? l1) (null? l2)) #f)
                      (else
                        (cond
                          ((and (atom? (car l1)) (atom? (car l2))) (and (eqan? (car l1) (car l2))
                                                                       (eqlist? (cdr l1) (cdr l2))))
                          ((or (atom? (car l1)) (atom? (car l2))) #f)
                          (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))))))) 
                   
   ;;define equal?  ||what's the difference between S-exp and list||or what's the hell is the S-exp
   ;;(equal? s1 s2) check whether s1 and s2 are the same S-expressions            
            (define equal?
              (lambda (s1 s2)
                (cond
                  ((and (atom? s1)(atom? s2))
                   (eqan? s1 s2))
                  ((or (atom? s1) (atom? s2)) #f)
                  (else (eqlist? s1 s2)))))
            
   ;;rewrite equal? with equal?
              (define eqlist2?
                (lambda (l1 l2)
                  (cond
                    ((and (null? l1) (null? l2)) #t)
                    ((or (null? l1) (null? l2)) #f)
                    (else
                     (and (equal? (car l1) (car l2))
                       (eqlist2? (cdr l1) (cdr l2)))))))
                
   ;;define rember+
   ;;(rember+ s l) s is S-exp and l is list of S-exp
              (define rember+
                (lambda (s l)
                  (cond
                    ((null? l) '())
                    ((equal? (car l) s) (cdr l))
                    (else (cons (car l)
                                (rember+ s (cdr l)))))))
                    
   ;;now think about eq?,=,eqan?,equal?,eqlist?
   ;;what's the relationship between them!
              
   ;;chapter six :shadows
   ;;about arithmetic expression
              
   ;;define numbered?
   ;;(numbered? aexp) aexp is short for arithmetic expression
   ;;this procedure check whether aexp is an aexp? --!
              (define numbered?
                (lambda (aexp)
                  (cond
                    ((atom? aexp) (number? aexp))
                    (else
                     (and (numbered? (car aexp))
                          (numbered? (car (cdr (cdr aexp)))))))))
   ;;a more complex numbered?
               (define numbered#?
                 (lambda (aexp)
                   (cond
                     ((atom? aexp) (number? aexp))
                     ((eq? (car (cdr aexp)) (quote +))
                      (and (numbered#? (car aexp))
                          (numbered#?
                             (car (cdr (cdr aexp))))))
                     ((eq? (car (cdr aexp)) (quote *))
                      (and (numbered#? (car aexp))
                           (numbered#?
                             (car (cdr (cdr aexp))))))
                     ((eq? (car (cdr aexp)) (quote exp))
                      (and (numbered#? (car aexp))
                           (numbered#?
                             (car (cdr (cdr aexp)))))))))
   
   ;; + represents a special operator while '+ represents just an atom!!
               
   ;;define value!   (value! nexp) returns the natural value of a numbered arithmetic expression!!     
               (define value!
                 (lambda (nexp)
                  (cond
                    ((atom? nexp) nexp)
                    ((eq? (car (cdr nexp)) (quote +))
                      (+ (value! (car nexp)) (value! (car (cdr (cdr nexp))))))
                    ((eq? (car (cdr nexp)) (quote *))
                      (* (value! (car nexp)) (value! (car (cdr (cdr nexp))))))
                    ((eq? (car (cdr nexp)) (quote expt))
                      (expt (value! (car nexp)) (value! (car (cdr (cdr nexp)))))))))
               
   ;;another value! whitch deals with another kind of nexp
               (define value
                 (lambda (nexp)
                   (cond
                     ((atom? nexp) nexp)
                     ((eq? (car nexp) (quote +))
                      (+ (value (cdr nexp))
                         (value (cdr (cdr nexp)))))
                     ((eq? (car nexp) (quote *))
                      (* (value (cdr nexp))
                         (value (cdr (cdr nexp)))))
                     (else
                      (expt (value (cdr nexp))
                            (value (cdr (cdr nexp))))))))
                           
   
   ;;now since the procedure "value" looks very trivial. think about high-level-procedure
               ;; get the operator
               (define operator
                 (lambda (aexp)
                   (car aexp)))
               
               ;;get the first part of aexp
               (define 1st-sub-exp
                 (lambda (aexp)
                   (cdr aexp)))
               
               ;;get the second part of aexp
               (define 2nd-sub-exp
                 (lambda (aexp)
                   (cdr (cdr aexp))))   ;;Remember the Eighth Commandment?:use help functions to abstract from representations!!
               
   ;;define set?  (set? lat) returns true #t iff lat is null or each atom in the lat appears only once 
               (define set?
                 (lambda (lat)
                   (cond 
                     ((null? lat) #t)
                     ((member? (car lat) (cdr lat)) #f)
                     (else (set? (cdr lat))))))
               
   ;;makeset?  (makeset lat) aims to make a set out of lat
               (define makeset
                 (lambda (lat)
                   (cond
                     ((null? lat) '())
                     ((member? (car lat) (cdr lat))
                        (makeset (cdr lat)))
                     (else (cons (car lat)
                                 (makeset (cdr lat)))))))
              
   ;;rewrite makeset use multirember!
               (define makeset!
                 (lambda (lat)
                   (cond
                     ((null? lat) '())
                     (else (cons (car lat)
                            (makeset! 
                             (multirember (car lat)
                              (cdr lat))))))))
   ;;define subset? (subset? (5 chicken wings) (5 hamburgers 2 pieces fried chicken and light duckling wings)) #t
               (define subset?
                 (lambda (set1 set2)
                   (cond 
                     ((null? set1) #t)
                     (else (and 
                              (member? (car set1) set2)
                              (subset? (cdr set1) set2))))))
   ;;define eqset?
               (define eqset?
                 (lambda (set1 set2)
                   (and (subset? set1 set2)
                        (subset? set2 set2))))
               
   ;;intersect?
               (define intersect?
                 (lambda (set1 set2)
                   (cond
                     ((null? set1) #f)
                     (else
                      (or (member? (car set1) set2)
                          (intersect? (cdr set1) set2))))))
               
   ;;intersect (intersect set1 set2) returns a set
               (define intersect
                 (lambda (set1 set2)
                   (cond
                     ((null? set1) '())
                     ((member? (car set1) set2)
                                  (cons (car set1)
                                   (intersect (cdr set1) set2)))
                     (else (intersect (cdr set1) set2)))))
               
   ;;define union combines 2 set together
               (define union
                 (lambda (set1 set2)
                   (cond
                     ((null? set1) set2)
                     ((member? (car set1) set2)
                        (union (cdr set1) set2))
                     (else (cons (car set1)
                                 (union (cdr set1) set2))))))
   ;;define xxx (xxx set1 set2) returns all the atoms in set1 that are not in set2
               (define xxx
                 (lambda (set1 set2)
                   (cond
                     ((null? set1) '())
                     ((member? (car set1) set2)
                       (xxx (cdr set1) set2))
                     (else (cons (car set1)
                              (xxx (cdr set1) set2))))))
               
   ;;define intersectall  (intersectall l-set)  
               (define intersectall
                 (lambda (l-set)
                   (cond
                     ((null? (cdr l-set)) (car l-set))
                     (else (intersect (car l-set)
                              (intersectall (cdr l-set)))))))
               
   ;;define a-pair? list with only 2 S-expressions
               (define a-pair?
                 (lambda (x)
                   (cond
                     ((atom? x) #f)
                     ((null? x) #f)
                     ((null? (cdr x)) #f)
                     ((null? (cdr (cdr x))) #t)
                     (else #f))))
   ;;define first second build
               (define first
                 (lambda (p)
                   (cond 
                     (else (car p)))))
               (define second
                 (lambda (p)
                   (cond
                     (else (car (cdr p))))))
               (define build
                 (lambda (s1 s2)
                   (cond
                     (else (cons s1
                             (cons s2 '()))))))
               (define third
                 (lambda (l)
                   (car (cdr (cdr l)))))
                      
   ;;rel--relation    fun--function
               (define fun?
                 (lambda (rel)
                   (set? (firsts rel))))
               
   ;;define revrel (revrel rel) returns another rel (revrel rel)=S(rel)
               (define revrel
                 (lambda (rel)
                   (cond
                   ((null? rel) '())
                   (else (cons (build
                                 (second (car rel))
                                 (first (car rel)))
                            (revrel (cdr rel)))))))  
   ;;build a help function to make revrel shorter
               (define revpair
                 (lambda (pair)
                   (build (second pair)
                          (first pair))))
        
   ;;define fullfun?
               (define fullfun?
                 (lambda (fun)
                   (fun? (revrel fun))))
               
   ;;define rember-f (rember-f test? a l) test? is a procedure returns #t/#f a is S-exp/atom l is lat/list
               (define rember-f
                 (lambda (test? a l)
                   (cond
                     ((null? l) '())
                     ((test? (car l) a) (cdr l))
                     (else (cons (car l)
                                 (rember-f test? a 
                                           (cdr l)))))))
   ;;an other rember-f
               (define rember-f*
                 (lambda (test?)
                   (lambda (a l)
                     (cond
                       ((null? l) '())
                       ((test? (car l) a) (cdr l))
                       (else (cons (car l)
                                   ((rember-f* test?) a (cdr l))))))))
   ;;define insertR-f
               (define insertR-f
                 (lambda (test?)
                   (lambda (new old l)
                     (cond 
                       ((null? l) '())
                       ((test? (car l) old)
                        (cons old (cons new (cdr l))))
                       (else (cons (car l)
                                   ((insertR-f test?) new old
                                                      (cdr l))))))))
   ;;define insertL-f
               (define insertL-f
                 (lambda (test?)
                   (lambda (new old l)
                     (cond 
                       ((null? l) '())
                       ((test? (car l) old)
                        (cons new (cons old (cdr l))))
                       (else (cons (car l)
                                   ((insertL-f test?) new old
                                                      (cdr l))))))))
   ;;define insert-g
               (define seqL
                 (lambda (new old l)
                   (cons new (cons old l))))
               (define seqR
                 (lambda (new old l)
                   (cons old (cons new l))))
               (define insert-g
                 (lambda (seg)
                   (lambda (new old l)
                     (cond 
                       ((null? l) '())
                       ((eq? (car l) old)
                        (seg new old (cdr l)))
                       (else (cons (car l)
                               ((insert-g seg) new old
                                 (cdr l))))))))           
         ;;insertL (define insertL (insert-g seqL))
         ;;insertR (define insertR (insert-g seqR))
         ;;Is it necessary to give names to seqL and seqR
   ;;define seqS
               (define seqS
                 (lambda (new old l)
                   (cons new l)))
         ;;subst (define subst (insert-g seqS))      
               
                (define seqrem
                  (lambda (new old l)
                    l))
         ;;rember (define rember 
         ;;         (lambda (a l)
         ;;            ((insert-g seqrem) #f a l)))
   ;;the Ninth Commandment:Abstract common patterns with a new function
   ;;define atom-to-function:build abstraction for "value"
                (define atom-to-function
                  (lambda (x)
                    (cond 
                      ((eq? x (quote +)) +)
                      ((eq? x (quote *)) *)
                      (else expt))))
   ;;now what is (atom-to-function (operator nexp)) -- the function +
                (define value*
                  (lambda (nexp)
                     (cond
                       ((atom? nexp) nexp)
                       (else
                        ((atom-to-function
                          (operator nexp))
                         (value* (1st-sub-exp nexp))
                         (value* (2nd-sub-exp) nexp))))))
                
   ;;multirember
                (define multirember-f
                  (lambda (test?)
                    (lambda (a lat)
                      (cond
                        ((null? lat) '())
                        ((test? a (car lat))
                         ((multirember-f test?) a
                             (cdr lat)))
                        (else (cons (car lat)
                                 ((multirember-f test?) a
                                        (cdr lat))))))))
  
   ;;((multirember-f test?) a lat) test? is eq? and a is tuna and lat 
   ;;is (shrimp salad tuna salad and tuna) 
   ;;让test？接受一个参数，这样(multirember-f test?)就只用接受一个参数
                
                (define a-friend
                  (lambda (x y)
                    (null? y)))
                (define multirember&co
                  (lambda (a lat col)
                    (cond
                      ((null? lat)
                       (col '() '()))
                      ((eq? (car lat) a)
                       (multirember&co a
                                       (cdr lat)
                                       (lambda (newlat seen)
                                               (col newlat
                                                    (cons (car lat) seen)))))
                      (else
                       (multirember&co a
                                       (cdr lat)
                                       (lambda (newlat seen)
                                         (col (cons (car lat) newlat)
                                              seen)))))))
     (define C
       (lambda (n)
         (cond
           ((one? n) 1)
           (else
            (cond
              ((even? n) (C (/ n 2)))
              (else (C (add1 (* 3 n)))))))))
               
     (define A
       (lambda (n m)
         (cond 
           ((zero? n) (add1 m))
           ((zero? m) (A (sub1 n) 1))
           (else (A (sub1 n)
                    (A n (sub1 m)))))))
               
      (define length!
        (lambda (l)
          (cond
            ((null? l) 0)
            (else (add1 (length! (cdr l)))))))
               
               
               
               
                      
                   
