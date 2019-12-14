(use-modules (ice-9 rdelim)
             (ice-9 ports)
             (srfi srfi-1))

(define input-file "inputs.txt")

(define (computer-parse program)
  (apply vector (map string->number (string-split program #\,))))

(define (computer-run program pc)
  (let ((elm (vector-ref program pc)))
    (if (= elm 99)
        program
        (let ((fn (case elm ((1) +) ((2) *)))
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

(define computer-program (call-with-input-file input-file read-line))

(define (part-one) (vector-ref (computer-exec computer-program) 0))

(display "Part one: ")
(display (part-one))
(newline)
