(use-modules (ice-9 rdelim)
             (ice-9 ports)
             (srfi srfi-1))

(define modules-masses
  (call-with-input-file "1.inputs.txt"
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

(define (day-1) (sum-fuels modules-masses 0 calc-fuel))

(define (fuel-for-fuel fuel acc)
  (let ((fl (fuel-for-mass fuel)))
    (if (< fl 0)
        acc
        (fuel-for-fuel fl (+ acc fl)))))

(define (day-2)
  (sum-fuels modules-masses 0
             (lambda (m)
               (let ((fuel-mass (fuel-for-mass m)))
                 (+ fuel-mass (fuel-for-fuel fuel-mass 0))))))
