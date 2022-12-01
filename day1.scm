(use-modules (ice-9 textual-ports)
             (ice-9 rdelim))

(define (parse-input-file filename)
  (let ((port (open-input-file filename)))
    (let parse-line ((groups '())
                     (current-group '()))
      (let ((current-line (read-line port)))
        (cond
         ((eof-object? current-line) groups)
         ((string=? "" current-line) (parse-line (cons current-group groups) '()))
         (else (parse-line groups (cons current-line current-group))))))))

(define (strs->numbers strs)
  (map string->number strs))

(define (list-sum list)
  (apply + list))

(define (sum-entry entry)
  (list-sum (strs->numbers entry)))

(define (part1 parsed-input)
  (let ((sums (map (lambda (sublist) (sum-entry sublist)) parsed-input)))
    (car (sort sums >))))

(display "Part 1: ")
(display (part1 (parse-input-file "inputs/day1")))
(newline)

(define (part2 parsed-input)
  (letrec ((sums (map (lambda (sublist) (sum-entry sublist)) parsed-input))
           (top-three (list-head (sort sums >) 3)))
    (apply + top-three)))

(display "Part 2: ")
(display (part2 (parse-input-file "inputs/day1")))
(newline)
