(use-modules
 (srfi srfi-1)
 (ice-9 textual-ports)
 (ice-9 rdelim))

(define (parse-input filename)
  (let ((port (open-input-file filename)))
    (let parse-line ((lines '()))
      (let ((current-line (read-line port)))
        (if (eof-object? current-line)
            lines
            (parse-line (cons current-line lines)))))))

(define (split-line line)
  (letrec ((chars-list (string->list line))
           (list-half-index (/ (length chars-list) 2))
           (first-half (list-head chars-list list-half-index))
           (second-half (list-tail chars-list list-half-index)))
    (list first-half second-half)))

(define (calculate-priority-value char)
  (let ((ascii-offset (if (char-upper-case? char) 38 96)))
    (- (char->integer char) ascii-offset)))

(define (calculate-priority first-half second-half)
  (let ((common-char (car (lset-intersection eqv? first-half second-half))))
    (calculate-priority-value common-char)))

(define (part1 lines)
  (let process ((sum 0)
                (entries lines))
    (if (null? entries)
        sum
        (letrec ((line (car entries))
                 (parts (split-line line))
                 (priority (calculate-priority (car parts) (cadr parts))))
          (process (+ sum priority) (cdr entries))))))

(display "day 3 part 1: ")
(display (part1 (parse-input "inputs/day3")))
(newline)

(define (part2 lines)
  (let process ((sum 0)
                (entries lines))
    (if (null? entries)
        sum
        (letrec ((lines (take entries 3))
                 (lines-chars (map (lambda (str) (string->list str)) lines))
                 (intersection (lset-intersection eqv?
                                                  (first lines-chars)
                                                  (second lines-chars)
                                                  (third lines-chars))))
          (process (+ sum (calculate-priority-value (car intersection))) (drop entries 3))))))

(display "day3 part 2: ")
(display (part2 (parse-input "inputs/day3")))
(newline)
