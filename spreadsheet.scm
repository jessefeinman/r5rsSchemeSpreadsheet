; Each cell has the format #(procedure '(lambda) cells-this-depends-on cells-depend-on-this value)
; Dependencies have the format ((x1 . y1)(x2 . y2) [...] (xn . yn))
; Ex. ((lambda (x y) (+ x y)) ((1 . 1)(1 . 2)))
; Where exp = (lambda (x y) (+ x y))
; and dependencies = ((1 . 1)(1 . 2))

;reduce function
(define (fold-right f init my-list)
     (cond ((null? my-list) init)
           (else (fold-right f (f init (car my-list)) (cdr my-list)))))
;filter function
(define (filter f my-list)
  (cond ((null? my-list) '())
        ((not (f (car my-list))) '())
        (else (append (list (car my-list))
                      (filter f (cdr my-list))))))

;size of spreadsheet
(define size 5)
;create new sheet of size n X n
(define (clear-and-change-size n) (set! size n)(clear-sheet))

;initialize the sheet
(define (initialize)
  ;creates a vector of vectors to make an size X size board
  (define (fill vector n)
    (cond ((eq? n size) vector)
          (else (vector-set! vector n (make-vector size))
                (fill vector (+ n 1)))))
  ;fills each cell of the vectors with a vector of length 5
  (define (cells vector x y)
    (cond ((and (eq? x 0) (eq? y size)) vector)
          ((and (eq? x size) (< y size))(cells vector 0 (+ y 1)))
          ((< x size)                   (vector-set! (vector-ref vector x) y (make-vector 5 '())) (cells vector (+ x 1) y))))
  (cells (fill (make-vector size) 0) 0 0))
;sets sheet equal to a newly initialized sheet
(define sheet (initialize))
;clear the current sheet
(define (clear-sheet) (set! sheet (initialize)))

;prints the sheet
;takes arguments:
;'values - shows values only (default)
;'lambdas - shows lambdas only
;'values 'lambdas - shows values when available otherwise shows lambdas
(define (display-sheet . args)
  (define (cell xy)
    (let ((x (get-x xy))
          (y (get-y xy)))
      (cond ((and (eq? x 0) (eq? y size)) (display "\n"))
            ((and (eq? x size) (< y size))(display "\n") (cell (cons 0 (+ y 1))))
            ((< x size)                   (display-cell xy) (cell (cons (+ x 1) y))))))
  (define (display-cell xy)
    (cond ((and lambdas values)
           (cond ((and (eq? (value xy) '() ) (not (eq? (exp-quote xy) '() )))
                  (display "(")
                  (display (exp-quote xy))
                  (display " ")
                  (display (this-depends-on xy))
                  (display ")")
                  (display "\t"))
                 (else (display (value xy)) (display "\t"))))
          (lambdas
                  (display "(")
                  (display (exp-quote xy))
                  (display " ")
                  (display (this-depends-on xy))
                  (display ")")
                  (display "\t"))
          (else (display (value xy)) (display "\t"))))
  (define lambdas #f)
  (define values #t)
  (cond ((and (member 'lambdas args)
              (member 'values args))
              (set! lambdas #t)
              (set! values #t))
        ((and (member 'lambdas args)
              (not (member 'values args)))
              (set! lambdas #t)
              (set! values #f)))
  (cell '(0 . 0)))

;get x or y value from dotted pair (x . y)
(define get-x car)
(define get-y cdr)
;get the components of a cell given it's (x . y) position
(define (exp-proc xy)             (vector-ref (get-cell xy) 0))
(define (exp-quote xy)            (vector-ref (get-cell xy) 1))
(define (this-depends-on xy)      (vector-ref (get-cell xy) 2))
(define (depends-on-this xy)      (vector-ref (get-cell xy) 3))
(define (value xy)                (vector-ref (get-cell xy) 4))
;set the components of a cell given it's (x . y) position
(define (set-exp-proc xy arg)       (vector-set! (get-cell xy) 0 arg))
(define (set-exp-quote xy arg)      (vector-set! (get-cell xy) 1 arg))
(define (set-this-depends-on xy arg)(vector-set! (get-cell xy) 2 arg))
(define (set-depends-on-this xy arg)(vector-set! (get-cell xy) 3 arg))
(define (set-value xy arg)          (vector-set! (get-cell xy) 4 arg))
;get the whole cell
(define (get-cell xy)              (vector-ref (vector-ref sheet (get-x xy)) (get-y xy)))
;set the value of a cell
(define (set-cell xy exp dependencies)
  (let ((old (list->vector (vector->list  (get-cell xy))))) ;saves old vector in case circular dependency
    ;remove this cell from depends-on-this in cells that this depends on, and recursively for those cells dependencies
    (update-depends-on-this xy 'remove)
    (set-exp-proc xy (eval exp (interaction-environment) ))
    (set-exp-quote xy exp)
    (set-this-depends-on xy dependencies)
    (set-depends-on-this xy (depends-on-this xy))
    ;checks for circular dependencies, restores cell to previous version if deteceted
    ;else set value fields
    (let ((circular-dependency (circular-dependency? xy)))
      (cond ((not (list? circular-dependency))
             ;if dependencies are set calculate the value for the cell
             (cond ((dependencies-set? xy) (set-value xy (apply (exp-proc xy) (get-values-list xy))))
                   ;else set value to '()
                   (else (set-value xy '())))
             ;add this cell to depends-on-this in cells that this depends on, and recursively for those cells dependencies
             (update-depends-on-this xy 'add)
             ;update values for cells that depends-on-this 
             (update-values xy)
             ;return nothing
             (values))
            ;if circular dependency, restore previous verison of the cell
            ;print a descriptive message
            ;show a trace of cell dependencies
            (else (vector-set! (vector-ref sheet (get-x xy)) (get-y xy) old)
                  (update-depends-on-this xy 'add)
                  (display "\nCircular dependency in cell ")
                  (display xy)
                  (display ", invalid input.\n")
                  (display "Trace of dependencies: \t")
                  (display circular-dependency)
                  (display "\n\n\n"))))))
  
;set, allows lambdas with arguments or constant value inputs
(define (set . args)
  (cond ((eq? (length args) 3) (set-cell (car args) (cadr args) (caddr args))) ;xy, lambda, dependencies
        ((eq? (length args) 2) (set-cell (car args) `(lambda () ,(cadr args)) '())))) ;xy, constant value

;get the values of the cells that xy-curr depends on
(define (get-values-list xy-curr)
  (map value (this-depends-on xy-curr)))

;circular-dependency?
(define (circular-dependency? xy-curr)
  (define visited '())
  (define (dependency-list xy)
    (cond ((member xy visited) (values))
          (else (set! visited (cons xy visited))
                (for-each dependency-list (this-depends-on xy)))))
  (for-each dependency-list (this-depends-on xy-curr))
  (cond ((member xy-curr visited) visited)
        (else #f)))

;returns true if the dependencies of the cell are set (are not '() null)
(define (dependencies-set? xy)
  (fold-right (lambda (init pair) (if (eq? (value pair) '()) #f init)) #t (this-depends-on xy)))

;update the depends-on-this field in all dells that xy-curr depends on
;'add adds xy-curr to depends-on-this in all cells xy-curr depends on
;'remove removes xy-curr from depends-on-this in all cells xy-curr depends on
(define (update-depends-on-this xy-curr arg)
  ;adds xy-curr to the cell if it is not present already
  (define (add-if-not-present xy)
    (cond ((not (member xy-curr (depends-on-this xy))) (set-depends-on-this xy (cons xy-curr (depends-on-this xy))))))
  ;removes xy-curr from the cell if it is present
  (define (remove-if-present xy)
    (set-depends-on-this xy (filter (lambda (pair) (not (equal? pair xy-curr))) (depends-on-this xy))))
  (cond ((eq? (this-depends-on xy-curr) '() ))
        ((eq? arg 'add) (for-each add-if-not-present (this-depends-on xy-curr)))
        ((eq? arg 'remove) (map remove-if-present (this-depends-on xy-curr)))))

;update the values in all cells that depend on xy-curr and recursively to the cells that depend on those cells
(define (update-values xy-curr)
  (for-each (lambda (xy)
              (cond ((dependencies-set? xy) (set-value xy (apply (exp-proc xy) (get-values-list xy)))
                                            (update-values xy))
                    (else (set-value xy '() ))))
            (depends-on-this xy-curr)))

;sample test data
(define (test)
  (set '(0 . 0) '(lambda (x) (+ x 3)) '((0 . 1)))
  (display-sheet)
  (set '(0 . 1) '5)
  (display-sheet)
  (set '(0 . 2) '(lambda () 11) '())
  (display-sheet)
  (set '(0 . 3) '(lambda (x y) (+ x y)) '((0 . 0)( 0 . 1)))
  (display-sheet)
  (set '(0 . 4) '(lambda (x y z) (+ z x y)) '((0 . 0) (0 . 2)(0 . 3)))
  (display-sheet)
  (set '(0 . 1) '20)
  (display-sheet)
  (set '(0 . 0) '(lambda (x) (+ x 3)) '((0 . 4)))
  (clear-sheet)
  (display-sheet)
  (set '(0 . 0) '(lambda (x y) (+ x 3)) '((0 . 1) (0 . 2)))
  (set '(0 . 1) '(lambda (x y z) (+ z x y)) '((0 . 0) (0 . 2)(0 . 3)))
  (set '(0 . 2) '(lambda (x) (+ x 3)) '((0 . 3)))
  (set '(0 . 3) '(lambda (x) (+ x 4)) '((0 . 4)))
  (set '(0 . 3) '(lambda (x) (+ x 3)) '((0 . 0)))
  (clear-sheet)
  (display-sheet)
  (set '(0 . 0) '(lambda (x y) (+ x 3)) '((0 . 1) (0 . 2)))
  (set '(0 . 1) '(lambda (x) (+ x 3)) '((1 . 0)))
  (set '(0 . 2) '(lambda (x) (+ x 4)) '((1 . 1)))
  (display-sheet)
  (set '(1 . 0) '5)
  (display-sheet)
  (set '(1 . 1) '3)
  (display-sheet 'lambdas)
  (set '(0 . 1) '(lambda (x) (+ x 3)) '((0 . 0)))
  (display-sheet 'lambdas)
  )
(test)