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

(define (day-1-a) (sum-fuels modules-masses 0 fuel-for-mass))

(define (fuel-for-fuel fuel acc)
  (let ((fl (fuel-for-mass fuel)))
    (if (< fl 0)
        acc
        (fuel-for-fuel fl (+ acc fl)))))

(define (day-1-b)
  (sum-fuels modules-masses 0
             (lambda (m)
               (let ((fuel-mass (fuel-for-mass m)))
                 (+ fuel-mass (fuel-for-fuel fuel-mass 0))))))

(define (computer-parse program)
  (apply vector (map string->number (string-split program #\,))))

(define (computer-run program pc)
  (let ((elm (vector-ref program pc)))
    (if (= elm 99)
        program
        (let* ((fn (case elm ((1) +) ((2) *)))
               (left-op (vector-ref program (+ pc 1)))
               (right-op (vector-ref program (+ pc 2)))
               (store-addr (vector-ref program (+ pc 3))))
          (vector-set! program store-addr
                       (fn (vector-ref program left-op)
                           (vector-ref program right-op)))
          (computer-run program (+ pc 4))))))

(define (computer-exec program)
  (let ((program (computer-parse program)))
    (vector-set! program 1 12)
    (vector-set! program 2 2)
    (computer-run program 0)))

(define computer-program (call-with-input-file "2.inputs.txt" read-line))

(define (day-2-a) (vector-ref (computer-exec computer-program) 0))
