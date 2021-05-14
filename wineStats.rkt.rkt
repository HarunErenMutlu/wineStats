#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;; first task
(define (mean elemList summ totalRow)
  (if (null? elemList)
    (/ summ totalRow)
    (mean (cdr elemList) (+ summ (car elemList)) totalRow)))

(define (variance elemList meanValue varianceSum n)
  (define square (lambda (x) (* x x)))
  (if (null? elemList)
   (/ varianceSum (- n 1))
   (variance (cdr elemList) meanValue (+ varianceSum (square (- (car elemList) meanValue))) n)))

(define (median lst)
  (let ((len (length lst)))
    (if (even? len)
        (/ (+ (list-ref lst (/ len 2))
              (list-ref lst (- (/ len 2) 1))) 2)
        (list-ref lst (/ (- len 1) 2)))))

(define (create-list elemList newList index)
  (if (null? elemList)
   newList
   (create-list (cdr elemList) (append newList (list (list-ref (car elemList) index))) index)))

(define (writeValues elemList nameList index)
  (define unsortedList (create-list elemList '() (+ index 1)))
  (define sortedList (sort unsortedList <))
  (let ((meanValue (exact->inexact (mean unsortedList 0 (length unsortedList)))))
        (let ((varianceValue (exact->inexact(variance unsortedList meanValue 0 (length unsortedList))))
        (medianValue (exact->inexact (median sortedList))))
  (display (list-ref nameList index))
  (displayln ":")
  (display "mean: ")
  (displayln meanValue)
  (display "variance: ")
  (displayln varianceValue)
  (display "median: ")
  (displayln medianValue)
  (displayln ""))))

(define (firstTask elemList nameList coloumnIndex)
  (if (> coloumnIndex 12)
      (display "")
      (begin
        (writeValues elemList names coloumnIndex)
        (firstTask elemList nameList (+ coloumnIndex 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; second task
(define (makeTrainingSet elemList newList len currentLen)
  (if (>= currentLen (* len 0.8))
      newList
      (makeTrainingSet (cdr elemList) (append newList  (list(car elemList))) len (+ 1 currentLen))))

(define (makeTestSet elemList trainingList newList)
  (let ((stopList (car(reverse trainingList))))
    (if (equal? stopList (car elemList))
        newList
        (makeTestSet (cdr elemList) trainingList (append newList (list(car elemList)))))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;third task
(define (calculateAccuracy testSet len correctGuess)
  (if (null? testSet)
      (exact->inexact(* 100 (/ correctGuess len)))
      (begin
        (if (equal? (+ 1 (random 3)) (car (car testSet)))
          (calculateAccuracy   (cdr testSet) len (+ 1 correctGuess))
          (calculateAccuracy   (cdr testSet) len correctGuess)))))

(define (thirdTask testSet)
  (define accuracy (calculateAccuracy testSet (length testSet) 0))
  (display "Accurracy is: %")
  (display accuracy))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; start point of main program  
(define myFile (file->lines "wine.data.txt"))
(define tempList (map (lambda (x) (regexp-split #px"," x)) myFile))
(define finalList (map (lambda(x) (map string->number x)) tempList))
(define shuffledList (shuffle finalList))
(define names (list "Alcohol" "Malic acid" "Ash" "Alcalinity of ash" "Magnesium" "Total phenols"
 	"Flavanoids" "Nonflavanoid phenols" "Proanthocyanins" "Color intensity" "Hue" "OD280/OD315 of diluted wines" "Proline"))

(firstTask finalList names 0)
(displayln "Test set and Train set are partitioning..")
(define trainSet (makeTrainingSet shuffledList '() (length shuffledList) 0))
(define testSet (makeTestSet (reverse shuffledList) trainSet '()))
(displayln "")
(thirdTask testSet)

