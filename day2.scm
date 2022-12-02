(use-modules (ice-9 textual-ports)
             (ice-9 rdelim))

(define (parse-input-file filename)
  (let ((port (open-input-file filename)))
    (let parse-line ((total 0))
      (let ((current-line (read-line port)))
        (if (eof-object? current-line)
            total
            (letrec ((round-moves (string->list current-line))
                     (enemy-move (car round-moves))
                     (player-move (caddr round-moves)))
              (parse-line (+ total (+ (move-value player-move) (outcome enemy-move player-move))))))))))

(define (move-value move-char)
  (cond
   ((eq? move-char #\A) 1)
   ((eq? move-char #\X) 1)
   ((eq? move-char #\B) 2)
   ((eq? move-char #\Y) 2)
   (else 3)))

(define (outcome enemy-move player-move)
  (let ((enemy-move-value (move-value enemy-move))
        (player-move-value (move-value player-move)))
    (let ((diff (modulo (- enemy-move-value player-move-value) 3)))
      (cond
       ;; draw
       ((eq? diff 0) 3) 
       ;; win
       ((eq? diff 1) 0)
       ;; lose
       ((eq? diff 2) 6)))))

(display "day 2 part 1: ")
(display (parse-input-file "inputs/day2"))
(newline)

