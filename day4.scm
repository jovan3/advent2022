(use-modules
 (srfi srfi-1)
 (ice-9 textual-ports)
 (ice-9 rdelim))

(string->number "3")

(define (extract-ranges range-str)
  (let ((range (string-split range-str #\-)))
    (map (lambda (strnum) (string->number strnum)) range)))

(define (process-line line-str)
  (let ((parts (string-split line-str #\,)))
    (list (extract-ranges (car parts)) (extract-ranges (cadr parts)))))

(define (file-lines)
  (letrec ((file-contents (call-with-input-file "inputs/day4" get-string-all))
           (lines (string-split file-contents #\newline)))
    (map (lambda (line) (process-line line)) lines)))

(define (range-inside-other? range1 range2)
  (let ((r1-from (car range1))
        (r1-to (cadr range1))
        (r2-from (car range2))
        (r2-to (cadr range2)))
    (or (and (>= r2-from r1-from)
             (<= r2-to r1-to))
        (and (>= r1-from r2-from)
             (<= r1-to r2-to)))))

(define (count-inside-ranges file-lines)
  (let count-ranges ((total 0)
                     (all-lines file-lines))
    (if (null? all-lines)
        total
        (letrec ((line (car all-lines))
                 (range1 (car line))
                 (range2 (cadr line)))
          (if (range-inside-other? range1 range2)
              (count-ranges (+ 1 total) (cdr all-lines))
              (count-ranges total (cdr all-lines)))))))
                         
(display "day 4 part 1: ")
(display (count-inside-ranges (file-lines)))
(newline)
