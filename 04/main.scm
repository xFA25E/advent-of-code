(define inputs-beg 158126)
(define inputs-end 624574)
(define inputs-length 6)

(define (valid-password? number)
  (let loop ((number number)
             (length inputs-length)
             (start-from 0)
             (prev-num -1)
             (doubles? #f))
    (if (zero? length)
        doubles?
        (let* ((base (expt 10 (1- length)))
               (mod (modulo number base))
               (last-num (/ (- number mod) base)))
          (if (<= start-from last-num 9)
              (loop mod (1- length) last-num last-num
                    (or doubles? (= last-num prev-num)))
              #f)))))

(define (part-one)
  (let loop ((number inputs-beg)
             (end inputs-end)
             (passes '()))
    (if (< end number)
        (length passes)
        (loop (1+ number) end
              (if (valid-password? number)
                  (cons number passes)
                  passes)))))

(display "Part one: ")
(display (part-one))
(newline)
