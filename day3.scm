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

(define (calculate-priority first-half second-half)
  (letrec ((common-char (car (lset-intersection eqv? first-half second-half)))
           (ascii-offset (if (char-upper-case? common-char) 38 96)))
    (- (char->integer common-char) ascii-offset)))

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
