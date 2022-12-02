(use-modules (ice-9 textual-ports)
             (ice-9 rdelim))

(define (day2 filename player-move-fn)
  (let ((port (open-input-file filename)))
    (let parse-line ((total 0))
      (let ((current-line (read-line port)))
        (if (eof-object? current-line)
            total
            (letrec ((round-moves (string->list current-line))
                     (enemy-move (car round-moves))
                     (player-move (player-move-fn enemy-move (caddr round-moves))))
              (parse-line (+ total
                             (+
                              (move-value player-move)
                              (outcome enemy-move player-move))))))))))

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
(display (day2 "inputs/day2" (lambda (_ player-move) player-move)))
(newline)

(define (part2-player-move enemy-move player-move)
  (letrec ((enemy-move-value (move-value enemy-move))
           (player-move-value (move-value player-move))

           (new-player-move-value (modulo
                                   (- enemy-move-value (- player-move-value)) ;lol
                                   3)))
    (cond
     ((eq? new-player-move-value 0) #\X)
     ((eq? new-player-move-value 1) #\Y)
     ((eq? new-player-move-value 2) #\Z))))

(display "day 2 part 2: ")
(display (day2 "inputs/day2" (lambda (enemy-move player-move) (part2-player-move enemy-move player-move))))
(newline)
