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

(define computer-program (call-with-input-file input-file read-line))

(define (set-inputs program pos-1 pos-2)
  (vector-set! program 1 pos-1)
  (vector-set! program 2 pos-2)
  program)

(define (part-one)
  (let ((program (computer-parse computer-program)))
    (vector-ref (computer-run (set-inputs program 12 2) 0) 0)))

(define (generate-inputs)
  (define (inner-1 n b acc)
    (if (<= 0 n)
        (inner-1 (1- n) b (cons (cons n b) acc))
        acc))

  (define (inner-2 n acc)
    (if (<= 0 n)
        (inner-2 (1- n) (inner-1 99 n acc))
        acc))

  (inner-2 99 '()))

(define part-two-result 19690720)

(define (part-two)
  (let ((inputs (generate-inputs)))
    (letrec ((inner
              (lambda (acc)
                (if (null? acc)
                    'not-found
                    (let ((a (caar acc))
                          (b (cdar acc))
                          (program (computer-parse computer-program)))
                      (if (= part-two-result
                             (vector-ref
                              (computer-run (set-inputs program a b) 0) 0))
                          (cons a b)
                          (inner (cdr acc))))))))
      (let ((result (inner inputs)))
        (+ (cdr result) (* 100 (car result)))))))

(display "Part one: ")
(display (part-one))
(newline)

(display "Part two: ")
(display (part-two))
(newline)
