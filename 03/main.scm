(use-modules (ice-9 rdelim)
             (ice-9 ports)
             (srfi srfi-1))

(define input-file "inputs.txt")

(define wires
  (call-with-input-file input-file
    (lambda (port)
      (cons (read-line port) (read-line port)))))

(define (point-create x y) (cons x y))
(define (point-x point) (car point))
(define (point-y point) (cdr point))

(define (vector-create x y) (cons x y))
(define (vector-x vector) (car vector))
(define (vector-y vector) (cdr vector))

;; R23, L34, U234, D234
(define (vector-parse s)
  (let ((length (string->number (substring s 1))))
    (case (string-ref s 0)
      ((#\U) (vector-create 0 length))
      ((#\D) (vector-create 0 (- length)))
      ((#\R) (vector-create length 0))
      ((#\L) (vector-create (- length) 0)))))

(define (vector-apply point vector)
  (point-create (+ (point-x point) (vector-x vector))
                (+ (point-y point) (vector-y vector))))

(define (distance point-1 point-2)
  (+ (abs (- (point-x point-1) (point-x point-2)))
     (abs (- (point-y point-1) (point-y point-2)))))

(define (segment-create point-1 point-2)
  (cons point-1 (cons point-2 (distance point-1 point-2))))

(define (segment-point-1 segment) (car segment))
(define (segment-point-2 segment) (cadr segment))
(define (segment-length segment) (cddr segment))

(define (closer-to-zero point-1 point-2)
  (if (or (and (positive? point-1) (positive? point-2))
          (and (negative? point-1) (negative? point-2)))
      (if (< (abs point-1) (abs point-2))
          point-1
          point-2)
      0))

(define (segment-intersect? segment-1 segment-2)
  (let* ((point-1-1-x (point-x (segment-point-1 segment-1)))
         (point-1-1-y (point-y (segment-point-1 segment-1)))
         (point-1-2-x (point-x (segment-point-2 segment-1)))
         (point-1-2-y (point-y (segment-point-2 segment-1)))
         (point-2-1-x (point-x (segment-point-1 segment-2)))
         (point-2-1-y (point-y (segment-point-1 segment-2)))
         (point-2-2-x (point-x (segment-point-2 segment-2)))
         (point-2-2-y (point-y (segment-point-2 segment-2)))
         (point-min-1-x (min point-1-1-x point-1-2-x))
         (point-max-1-x (max point-1-1-x point-1-2-x))
         (point-min-1-y (min point-1-1-y point-1-2-y))
         (point-max-1-y (max point-1-1-y point-1-2-y))
         (point-min-2-x (min point-2-1-x point-2-2-x))
         (point-max-2-x (max point-2-1-x point-2-2-x))
         (point-min-2-y (min point-2-1-y point-2-2-y))
         (point-max-2-y (max point-2-1-y point-2-2-y)))

    (cond
     ;; on the same row
     ((= point-1-1-x point-1-2-x point-2-1-x point-2-2-x)
      (cond ((<= point-min-1-y point-min-2-y point-max-1-y)
             (if (<= point-min-1-y point-max-2-y point-max-1-y)
                 (point-create point-1-1-x (closer-to-zero point-min-2-y
                                                           point-max-2-y))
                 (point-create point-1-1-x (closer-to-zero point-min-2-y
                                                           point-max-1-y))))
            ((<= point-min-1-y point-max-2-y point-max-1-y)
             (point-create point-1-1-x (closer-to-zero point-min-1-y
                                                       point-max-2-y)))
            ((<= point-min-2-y point-min-1-y point-max-2-y)
             (if (<= point-min-2-y point-max-1-y point-max-2-y)
                 (point-create point-1-1-x (closer-to-zero point-min-1-y
                                                           point-max-1-y))
                 (point-create point-1-1-x (closer-to-zero point-min-1-y
                                                           point-max-2-y))))
            ((<= point-min-2-y point-max-1-y point-max-2-y)
             (point-create point-1-1-x (closer-to-zero point-min-2-y
                                                       point-max-1-y)))
            (else #f)))
     ;; on the same column
     ((= point-1-1-y point-1-2-y point-2-1-y point-2-2-y)
      (cond ((<= point-min-1-x point-min-2-x point-max-1-x)
             (if (<= point-min-1-x point-max-2-x point-max-1-x)
                 (point-create (closer-to-zero point-min-2-x point-max-2-x)
                               point-1-1-y)
                 (point-create (closer-to-zero point-min-2-x point-max-1-x)
                               point-1-1-y)))
            ((<= point-min-1-x point-max-2-x point-max-1-x)
             (point-create (closer-to-zero point-min-1-x point-max-2-x)
                           point-1-1-y))
            ((<= point-min-2-x point-min-1-x point-max-2-x)
             (if (<= point-min-2-x popint-max-1-x point-max-2-x)
                 (point-create (closer-to-zero point-min-1-x point-max-1-x)
                               point-1-1-y)
                 (point-create (closer-to-zero point-min-1-x point-max-2-x)
                               point-1-1-y)))
            ((<= point-min-2-x point-max-1-x point-max-2-x)
             (point-create (closer-to-zero point-min-2-x point-max-1-x)
                           point-1-1-y))
            (else #f)))
     ;; first vertical, second horizontal
     ((and (= point-1-1-x point-1-2-x) (= point-2-1-y point-2-2-y))
      (if (and (<= point-min-1-y point-2-1-y point-max-1-y)
               (<= point-min-2-x point-1-1-x point-max-2-x))
          (point-create point-1-1-x point-2-1-y)
          #f))
     ;; first horizontal, second vertical
     ((and (= point-1-1-y point-1-2-y) (= point-2-1-x point-2-2-x))
      (if (and (<= point-min-1-x point-2-1-x point-max-1-x)
               (<= point-min-2-y point-1-1-y point-max-2-y))
          (point-create point-2-1-x point-1-1-y)
          #f))

     (else #f))))

(define (distance-to-origin point)
  (+ (abs (point-x point)) (abs (point-y point))))

(define (wire-parse s)
  (let loop ((wire (string-split s #\,)) (point (point-create 0 0)) (acc '()))
    (if (null? wire)
        (reverse! acc)
        (let ((next-point (vector-apply point (vector-parse (car wire)))))
          (loop (cdr wire)
                next-point
                (cons (segment-create point next-point) acc))))))

(define (find-intersections wire-1 length-1 wire-2 intersections)
  (if (null? wire-1)
      intersections
      (find-intersections
       (cdr wire-1)
       (+ length-1 (segment-length (car wire-1)))
       wire-2
       (let loop ((segment (car wire-1))
                  (wire wire-2)
                  (length-2 0)
                  (intersections intersections))
         (if (null? wire)
             intersections
             (let ((intersection (segment-intersect? segment (car wire))))
               (if intersection
                   (loop segment
                         (cdr wire)
                         (+ length-2 (segment-length (car wire)))
                         (cons (cons intersection
                                     (+ length-1 length-2
                                        (distance (segment-point-1 segment)
                                                  intersection)
                                        (distance (segment-point-1 (car wire))
                                                  intersection)))
                               intersections))
                   (loop segment
                         (cdr wire)
                         (+ length-2 (segment-length (car wire)))
                         intersections))))))))

(define (find-min-intersection wire-1 wire-2)
  (apply min (filter! (lambda (a) (not (zero? a)))
                      (map (lambda (s) (distance-to-origin (car s)))
                           (find-intersections
                            (wire-parse wire-1) 0
                            (wire-parse wire-2) '())))))

(let ((w1 "R75,D30,R83,U83,L12,D49,R71,U7,L72")
      (w2 "U62,R66,U55,R34,D71,R55,D58,R83"))
  (find-min-intersection w1 w2))
;; should be 159

(let ((w1 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
      (w2 "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))
  (find-min-intersection w1 w2))
;; should be 135

(define (part-one)
  (let ((w1 (car wires))
        (w2 (cdr wires)))
    (find-min-intersection w1 w2)))

(define (find-min-intersection-path wire-1 wire-2)
  (apply min (filter! (lambda (a) (not (zero? a)))
                      (map cdr (find-intersections
                                (wire-parse wire-1) 0
                                (wire-parse wire-2) '())))))

(let ((w1 "R75,D30,R83,U83,L12,D49,R71,U7,L72")
      (w2 "U62,R66,U55,R34,D71,R55,D58,R83"))
  (find-min-intersection-path w1 w2))
;; should be 610

(let ((w1 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
      (w2 "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))
  (find-min-intersection-path w1 w2))
;; should be 410

(define (part-two)
  (let ((w1 (car wires))
        (w2 (cdr wires)))
    (find-min-intersection-path w1 w2)))

(display "Part one: ")
(display (part-one))
(newline)

(display "Part two: ")
(display (part-two))
(newline)
