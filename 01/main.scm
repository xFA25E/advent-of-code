(use-modules (ice-9 rdelim)
             (ice-9 ports)
             (srfi srfi-1))

(define input-file "inputs.txt")

(define modules-masses
  (call-with-input-file input-file
    (lambda (port)
      (letrec ((get-lines
                (lambda (acc)
                  (let ((line (read-line port)))
                    (if (eof-object? line)
                        acc
                        (get-lines (cons (string->number line) acc)))))))
        (get-lines '())))))

(define (fuel-for-mass mass) (- (floor (/ mass 3)) 2))

(define (sum-fuels masses acc fn)
  (if (null? masses)
      acc
      (sum-fuels (cdr masses) (+ acc (fn (car masses))) fn)))

(define (part-one) (sum-fuels modules-masses 0 fuel-for-mass))

(define (fuel-for-fuel fuel acc)
  (let ((fl (fuel-for-mass fuel)))
    (if (< fl 0)
        acc
        (fuel-for-fuel fl (+ acc fl)))))

(define (part-two)
  (sum-fuels modules-masses 0
             (lambda (m)
               (let ((fuel-mass (fuel-for-mass m)))
                 (+ fuel-mass (fuel-for-fuel fuel-mass 0))))))


(display "Part one: ")
(display (part-one))
(newline)

(display "Part two: ")
(display (part-two))
(newline)
