;;;; .author Brittany Reedman & Kevin Bohinski
;;;; .date 11/2/2016
;;;; .affiliation CSC 435 Programming Languages
;;;; .title Project 2: Part 2, Parser Generator

;;;; Section: Overarching functions & input.
;;;; This section contains functions that are the overarching functions

; Input from spec...
(define calc-gram
  '(("P"  ("SL" "$$"))
    ("SL" ("S" "SL") ())
    ("S"  ("id" ":=" "E") ("read" "id") ("write" "E"))
    ("E"  ("T" "TT"))
    ("T"  ("F" "FT"))
    ("TT" ("ao" "T" "TT") ())
    ("FT" ("mo" "F" "FT") ())
    ("ao" ("+") ("-"))
    ("mo" ("*") ("/"))
    ("F"  ("id") ("num") ("(" "E" ")"))
    ))

;; This function generates the final output as per spec.
;; .parameter grammar
;; .example (parse-table calc-gram)
;; .returns A parse table of predict sets
(define (parse-table grammar)
  
  ;; This lambda function generates the first sets.
  ;; Adds epsilon to first set for all prods that go to epsilon.
  ;; Cycles through grammar productions to get all first sets.
  ;; .parameter grammar
  ;; .example (generate-first-set calc-gram)
  ;; .returns The first sets
  (define (generate-first-set grammar)
    
    ;; This adds epsilon to the first set of all non terms who have a production that is epsilon.
    ;; .parameter first-set
    ;; .parameter grammar
    ;; .example (create-first-template '() grammar)
    ;; .returns Updated set
    (define (create-first-template first-set grammar)
      
      ;; Returns if a production contains epsilon.
      ;; .parameter production
      ;; .example (production-contains-epsilon? (cdr x))
      ;; .returns T/F if a production contains epsilon
      (define (production-contains-epsilon? production)
        (if (null? production) #f
            (if (null? (car production)) #t
                (production-contains-epsilon? (cdr production)))))
      
      (if (null? grammar) first-set
          (let ((x (car grammar)))
            (if (production-contains-epsilon? (cdr x))
                (create-first-template (append first-set (list (list (car x) '("epsilon")))) (cdr grammar))
                (create-first-template (append first-set (list (list (car x) '()))) (cdr grammar))))))
    
    (let ((first-set (create-first-template '() grammar)))
      (cycle-g first-set grammar first-set grammar #t '())))
  
  ;; This function generates the follow sets.
  ;; .parameter grammar
  ;; .example (generate-follow-set calc-gram first-set)
  ;; .returns The follow sets
  (define (generate-follow-set grammar first-set)
    (let ((follow-set (create-follow-template '() grammar #t)))
      (cycle-g follow-set grammar follow-set grammar #f first-set)))
  
  (let* ((first-set (generate-first-set grammar))
         (follow-set (generate-follow-set grammar first-set)))
    (gen-ps grammar '() first-set follow-set '() grammar #t)))

;;;; Section: Cycling helper functions.
;;;; This section contains helper functions that cycle through data.

;; This function cycles through all nonterminals in the grammar and calls a function
;; to look at each prodection for every nonterminal. If fi-or-fo is true,
;; the first set will be calculated, otherwise, the follow set will be calculated.
;; Checks to see if there was any change in the first/follow set after running through
;; everything, if so, first/follow sets are calculated again, otherwise, the set is returned.
;; .parameter set
;; .parameter grammar
;; .parameter original-set
;; .parameter original-grammar
;; .parameter fi-or-fo
;; .parameter first-set
;; .example (cycle-g first-set grammar first-set grammar #t first-set)
;; .returns First/Follow set depending on fi-or-fo
(define (cycle-g set grammar original-set original-grammar fi-or-fo first-set)
  (if (null? grammar)
      (if (equal? set original-set) set
          (cycle-g set original-grammar set original-grammar fi-or-fo first-set))
      (let ((car-grammar (car grammar)))
        (cycle-g (cycle-p (car car-grammar) (cdr car-grammar) set fi-or-fo first-set)
                 (cdr grammar) original-set original-grammar fi-or-fo first-set))))

;; This function cycles through every production for a given nonterminal.
;; .parameter symbol
;; .parameter production
;; .parameter set
;; .parameter fi-or-fo
;; .parameter first-set
;; .example (cycle-p symbol other-prods set fi-or-fo first-set)
;; .returns First/Follow set depending on fi-or-fo
(define (cycle-p symbol production set fi-or-fo first-set)
  (if (null? production) set
      (let ((curr-prod (car production)) (rest-of-prods (cdr production)))
        (if (null? curr-prod)
            (cycle-p symbol rest-of-prods set fi-or-fo first-set)
            (cycle-p
             symbol
             rest-of-prods
             (cycle-els symbol curr-prod set fi-or-fo first-set)
             fi-or-fo
             first-set)
            ))))

;; This function cycles through every element for a given production.
;; .parameter symbol
;; .parameter curr-prod
;; .parameter set
;; .parameter fi-or-fo
;; .parameter first-set
;; .example (cycle-els symbol curr-prod set fi-or-fo first-set)
;; .returns First/Follow set depending on fi-or-fo
(define (cycle-els symbol curr-prod set fi-or-fo first-set)
  (if (null? curr-prod) set
      (if (equal? fi-or-fo #t)
          (let* ((first-car-production (get-first (car curr-prod) (cdr set)))
                 (new-first-set (add-to-first-set symbol set first-car-production '()))
                 )
            (if (goes-to-epsilon? first-car-production)
                (cycle-els symbol (cdr curr-prod) new-first-set fi-or-fo first-set)
                (cycle-els symbol '() new-first-set fi-or-fo first-set)))
          (cycle-els symbol
                     '()
                     (follows-step-3
                      symbol
                      curr-prod
                      (follows-step-2
                       symbol
                       curr-prod
                       (follows-step-1
                        curr-prod
                        set
                        first-set)
                       first-set))
                     fi-or-fo
                     first-set))))

;;;; Section: Set and Grammar helper functions.
;;;; This section contains helper functions that perform or
;;;; return various set and grammar operations/information.

;; This returns true if symbol is a nonterminal and false otherwise.
;; .parameter symbol
;; .parameter set
;; .example (nonterminal? symbol set)
;; .returns T/F if a given symbol is nonterminal
(define (nonterminal? symbol set)
  (if (null? set) #f
      (if (string=? (car (car set)) symbol) #t
          (nonterminal? symbol (cdr set)))))

;; This returns the union of two sets.
;; .parameter set1
;; .parameter set2
;; .parameter remove-epsilon
;; .example (union-of-sets set1 y #t)
;; .returns A set which is the union of the parameters (epsilon may be removed)
(define (union-of-sets set1 set2 remove-epsilon)
  (if (null? set2) set1
      (let ((car-set2 (car set2)) (cdr-set2 (cdr set2)))
        
        ;; This lambda function returns if a set contains a given element.
        ;; .parameter symbol
        ;; .parameter set2
        ;; .example (set-contains? x set1)
        ;; .returns T/F if the set contains an element
        (define (set-contains? symbol set2)
          (if (null? set2) #f
              (if (string=? symbol (car set2)) #t
                  (set-contains? symbol (cdr set2)))))
        
        (if (or (set-contains? car-set2 set1)
                (and (string=? "epsilon" car-set2) (equal? remove-epsilon #t)))
            (union-of-sets set1 cdr-set2 remove-epsilon)
            (union-of-sets (append set1 (list car-set2)) cdr-set2 remove-epsilon)))))

;;;; Section: First set functions.
;;;; This section contains helper functions that generate first sets.

;; This adds elements, indicated by set-to-add, to the first set of symbol
;; .parameter symbol
;; .parameter first-set
;; .parameter set-to-add
;; .parameter new-set
;; .example (add-to-first-set symbol set x '())
;; .returns The updated set
(define (add-to-first-set symbol first-set set-to-add new-set)
  (if (null? first-set) new-set
      (let ((car-car-first-set (car (car first-set))) (y (cdr first-set)) (cadr-car-firstSet (cadr (car first-set))))
        (if (string=? symbol car-car-first-set)
            (add-to-first-set
             symbol
             y
             set-to-add
             (append new-set
                     (list
                      (append
                       (list car-car-first-set)
                       (list (union-of-sets cadr-car-firstSet set-to-add #f))))))
            (add-to-first-set
             symbol
             y
             set-to-add
             (append new-set
                     (list
                      (append
                       (list car-car-first-set) (list cadr-car-firstSet)))))))))

;; This returns the first set of symbol
;; .parameter symbol
;; .parameter first-set
;; .example (get-first (car curr-prod) (cdr set))
;; .returns The first set of symbol
(define (get-first symbol first-set)
  
  ;; This returns the first set at symbol from get-first.
  ;; .parameter symbol
  ;; .parameter first-set
  ;; .example (check-first-set symbol first-set)
  ;; .returns The first set at symbol from get-first
  (define (check-first-set symbol first-set)
    (if (null? first-set) '()
        (let ((car-first-set (car first-set)))
          (if (string=? symbol (car car-first-set)) (cdr car-first-set)
              (check-first-set symbol (cdr first-set))))))
  
  (if (not (nonterminal? symbol first-set)) (list symbol)
      (let ((symbol-first-set (check-first-set symbol first-set)))
        (if (null? symbol-first-set) '()
            (car symbol-first-set)))))

;; This returns the if the element goes to epsilon.
;; .parameter first-set
;; .example (goes-to-epsilon? x)
;; .returns If the element goes to epsilon
(define (goes-to-epsilon? first-set)
  (if (null? first-set) #f
      (if (string=? "epsilon" (car first-set)) #t
          (goes-to-epsilon? (cdr first-set)))))

;;;; Section: Follow set functions.
;;;; This section contains helper functions that generate follow sets.

;; This returns a list formatted as a follow-set.
;; .parameter follow-set
;; .parameter grammar
;; .parameter start-of-grammar
;; .example (create-follow-template '() grammar #t)
;; .returns Updated set
(define (create-follow-template follow-set grammar start-of-grammar)
  (if (null? grammar) follow-set
      (let ((car-car-grammar (car (car grammar))) (cdr-grammar (cdr grammar)))
        (if (equal? start-of-grammar #t)
            (create-follow-template (append follow-set (list (list car-car-grammar '("$$")))) cdr-grammar #f)
            (create-follow-template (append follow-set (list (list car-car-grammar '()))) cdr-grammar #f)))))

;; This function performs the first step in follow set generation.
;; .parameter production
;; .parameter follow-set
;; .parameter first-set
;; .example (follows-step-1 curr-prod set first-set)
;; .returns Set that satisfies the first rule from slides
(define (follows-step-1 production follow-set first-set)
  (if (null? production) follow-set
      (let ((car-production (car production)) (cdr-production (cdr production)))
        (if (nonterminal? (car production) follow-set)
            (if (null? (cdr production)) follow-set
                (follows-step-1 cdr-production
                                (add-to-follow-set
                                 car-production
                                 follow-set
                                 (get-first (cadr production) first-set)
                                 '())
                                first-set))
            (follows-step-1 cdr-production follow-set first-set)))))

;; This function performs the second step in follow set generation.
;; .parameter symbol
;; .parameter curr-prod
;; .parameter follow-set
;; .parameter first-set
;; .example (follows-step-2 symbol curr-prod (follows-step-1 curr-prod set first-set)
;; .returns Set that satisfies the second rule from slides
(define (follows-step-2 symbol curr-prod follow-set first-set)
  (if (null? curr-prod) follow-set
      (if (null? (cdr curr-prod)) follow-set
          (if (and (nonterminal? (cadr curr-prod) follow-set)
                   (equal? (goes-to-epsilon? (get-first (cadr curr-prod) first-set)) #t))
              (follows-step-2
               symbol
               (cdr curr-prod)
               (add-to-follow-set (car curr-prod) follow-set (get-follow symbol follow-set) '())
               first-set)
              (follows-step-2 symbol (cdr curr-prod) follow-set first-set)))))

;; This function adds the last nonterminal to a follow set.
;; .parameter symbol
;; .parameter production
;; .parameter follow-set
;; .example (follows-step-3 symbol (cdr production) follow-set)
;; .returns Set that satisfies the third rule from slides
(define (follows-step-3 symbol production follow-set)
  (if (null? production) follow-set
      (if (null? (cdr production))
          (follows-step-3
           symbol
           (cdr production)
           (add-to-follow-set (car production) follow-set (get-follow symbol follow-set) '()))
          (follows-step-3 symbol (cdr production) follow-set))))

;; This function returns a follow set.
;; .parameter symbol
;; .parameter follow-set
;; .example (get-follow symbol follow-set)
;; .returns The requested follow set
(define (get-follow symbol follow-set)
  
  ;; This checks the follow set at symbol.
  ;; .parameter symbol
  ;; .parameter follow-set
  ;; .example (check-follow-set symbol follow-set)
  ;; .returns Check operation
  (define (check-follow-set symbol follow-set)
    (if (null? follow-set) '()
        (let ((car-follow-set (car follow-set)))
          (if (string=? symbol (car car-follow-set)) (cdr car-follow-set)
              (check-follow-set symbol (cdr follow-set))))))
  
  (if (not (nonterminal? symbol follow-set)) '()
      (let ((symbol-follow-set (check-follow-set symbol follow-set)))
        (if (null? symbol-follow-set) '()
            (car symbol-follow-set)))))

;; This function adds a set to a given follow set.
;; .parameter symbol
;; .parameter follow-set
;; .parameter set-to-add
;; .parameter new-set
;; .example (add-to-follow-set (car curr-prod) follow-set (get-follow symbol follow-set) '())
;; .returns Total set
(define (add-to-follow-set symbol follow-set set-to-add new-set)
  (if (null? follow-set) new-set
      (let ((x (car follow-set)) (y (cdr follow-set)))
        (if (string=? symbol (car (car follow-set)))
            (add-to-follow-set
             symbol
             y
             set-to-add
             (append new-set (list (append
                                    (list (car x)) (list (union-of-sets (car (cdr x)) set-to-add #t))))))
            (add-to-follow-set
             symbol
             y
             set-to-add
             (append new-set (list(append (list (car x)) (list (car (cdr x)))))))))))

;;;; Section: Predict set functions.
;;;; This section contains helper functions that that generate predict sets.
;;;; https://www.usna.edu/Users/cs/roche/courses/f11si413/c10/ff.pdf

;; This function generates the predict sets.
;; .parameter grammar
;; .parameter predict-set
;; .parameter first-set
;; .parameter follow-set
;; .parameter original-ps
;; .parameter original-grammar
;; .parameter first-iteration
;; .example (gen-ps original-grammar '() first-set follow-set predict-set original-grammar #f)
;; .returns The predict sets
(define (gen-ps grammar predict-set first-set follow-set original-ps original-grammar first-iteration)
  (if (null? grammar)
      (if (and (equal? first-iteration #f) (equal? original-ps predict-set)) predict-set
          (gen-ps original-grammar '() first-set follow-set predict-set original-grammar #f))
      (gen-ps (cdr grammar)
              (template-ps grammar first-set follow-set predict-set original-ps)
              first-set follow-set original-ps original-grammar first-iteration)))

;; This function generates the predict set template.
;; .parameter grammar
;; .parameter first-set
;; .parameter follow-set
;; .parameter predict-set
;; .parameter original-ps
;; .example (template-ps grammar first-set follow-set predict-set original-ps)
;; .returns The predict set template
(define (template-ps grammar first-set follow-set predict-set original-ps)
  (append predict-set
          (list (append
                 (list (car (car grammar)))
                 (cycle-ps (cdr (car grammar))
                           first-set
                           (get-follow (car (car grammar)) follow-set)
                           '()
                           original-ps)))))

;; This function cycles through and generates the predict sets.
;; .parameter production
;; .parameter first-set
;; .parameter follow-set
;; .parameter predict-set
;; .parameter total-ps
;; .example (cycle-ps (cdr (car grammar)) first-set (get-follow (car (car grammar)) follow-set) '() original-ps)
;; .returns A predict set
(define (cycle-ps production first-set follow-set predict-set total-ps)
  (if (null? production) predict-set
      (if (null? (car production))
          (cycle-ps (cdr production)
                    first-set
                    follow-set
                    (append predict-set (list (list follow-set (car production))))
                    total-ps)
          (if (nonterminal? (car (car production)) first-set)
              (cycle-ps (cdr production)
                        first-set
                        follow-set
                        (append predict-set
                                (list
                                 (list
                                  (rule3-ps (car (car production)) first-set follow-set total-ps)
                                  (car production)))) total-ps)
              (if (nonterminal? (car (car production)) first-set)
                  (cycle-ps (cdr production) first-set follow-set predict-set total-ps)
                  (cycle-ps (cdr production) first-set follow-set
                            (append predict-set
                                    (list (list (list
                                                 (car (car production))) (car production)))) total-ps))))))

;; This function performs the third rule of predict set generation.
;; .parameter symbol
;; .parameter first-set
;; .parameter follow-set
;; .parameter total-ps
;; .example (rule3-ps (car (car production)) first-set follow-set total-ps)
;; .returns A predict set
(define (rule3-ps symbol first-set follow-set total-ps)
  
  ;; This returns the predict set of symbol
  ;; .parameter symbol
  ;; .parameter predict-set
  ;; .example (get-ps symbol (cdr predict-set))
  ;; .returns The predict set of symbol
  (define (get-ps symbol predict-set)
    
    ;; This returns the union of two predict sets.
    ;; .parameter predict-set
    ;; .parameter union
    ;; .example (union-ps (cdr (car predict-set)) '())
    ;; .returns A predict set with the union set
    (define (union-ps predict-set union)
      (if (null? predict-set) union
          (union-ps (cdr predict-set) (union-of-sets (car (car predict-set)) union #f))))
    
    (if (null? predict-set) '()
        (if (string=? symbol (car (car predict-set)))
            (union-ps (cdr (car predict-set)) '())
            (get-ps symbol (cdr predict-set)))))
  
  (union-of-sets
   (if (not (equal? (goes-to-epsilon? (get-first symbol first-set)) #t)) '() follow-set)
   (get-ps symbol total-ps) #f))