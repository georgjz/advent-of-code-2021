;;; Part 1
;;; ------
;; helpers
(define input
  (letrec ((input-file (open-input-file "Input.txt"))
          (R (lambda (ls)
               (let ((next (read input-file)))
                 (if (eof-object? next)
                     ls
                     (R (cons next ls)))))))
    (reverse (R '()))))

(define all-but-last
  (lambda (ls)
    (reverse (cdr (reverse ls)))))

;; solution
(define depth-diffs
  (lambda (depths)
    (let ((minuends (all-but-last depths))
          (subtrahends (cdr depths)))
      (map - minuends subtrahends))))

(define depth-increases
  (lambda (diffs)
    (fold-right (lambda (d ds)
                  (if (< d 0)
                      (+ 1 ds)
                      ds))
                0
                diffs)))

(define solution-part1 (depth-increases (depth-diffs input)))

;;; Part 2
;;; ------
(define depth-triple-sums
  (let ((summands1 (all-but-last (all-but-last input)))
        (summands2 (all-but-last (cdr input)))
        (summands3 (cddr input)))
    (map + summands1 summands2 summands3)))

(define solution-part2 (depth-increases (depth-diffs depth-triple-sums)))

(display (format "Part 1: ~s~%Part 2: ~s"
                 solution-part1
                 solution-part2))