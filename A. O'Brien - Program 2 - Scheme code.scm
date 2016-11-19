;***********************************************************************************************************
;AUTHOR: Aaron O'Brien		    	           	          				           *
;PROGRAM PURPOSE: To generate a new sorted list                       				           *
;	          consisting of like-elements derived from two lists.                                       *
;REFERENCES: "The Little Schemer" 4th Edition by Friedman & Felleisen 					   *
;													   *
; *1ST FUNCTION* PARTITION									           *
;	PURPOSE: TO THREE-WAY PARTITION A LIST INTO SUBLISTS TO MAKE COMPARING SIMPLE 		           *
;		 & PAVE WAY TO A QUICKER SORTING PROCESS.					           *
;													   *
; *2ND FUNCTION* QUICKSORT										   *
;	PURPOSE: TO SORT A LIST OF NUMBERS FROM SMALLEST TO GREATEST					   *
;                WITH THE HELP OF LOCAL VARIABLE "theMeme" 						   *
;                WHICH IS ASSIGNED TO THE FIRST ELEMENT FROM THE LIST.                                     *
;													   *
; *3RD FUNCTION* ORDERINTERSECTION								           *
;	PURPOSE: TO INTERSECT TWO LISTS' LIKE-ELEMENTS. 						   *
;  *TEST CODE AT BOTTOM*					                                           *
;													   *
;***********************************************************************************************************	
			
(define (partition compare L1)
    (cond
         ((null? L1) '())
         ((compare (car L1)) (cons (car L1) (partition compare (cdr L1))))
         (else (partition compare (cdr L1)))))

(define (quicksort theList)
    (cond
         ((null? theList) '())
         (else (let ((theMeme (car theList)))
            (append (append (quicksort (partition (lambda (x) (< x theMeme)) theList))
                       (partition (lambda (x) (= x theMeme)) theList))
                    (quicksort (partition (lambda (x) (> x theMeme)) theList)))))))
                   
(define OrderIntersection
    (lambda (L1 L2)
         (cond ((null? L1) '())
         ((member (car L1) L2)
           (cons (car L1)
            (OrderIntersection (cdr L1) L2)))
         (else (OrderIntersection (cdr L1) L2)))))

;TESTING QUICKSORT & INTERSECTION WITH TWO POPULATED LISTS.
(quicksort (OrderIntersection 
    (list 5 12 22 7 12 8 75 66 13 77 77) 
    (list 5 4 7 4 21 5 65 4 12 5 45 8 66 13)))                  
