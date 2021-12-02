;;; Part 1
;;; ------
;; helpers
(define input
  (letrec ((input-file (open-input-file "Input.txt"))
          (R (lambda (ls)
               (let* ((dir (read input-file))
                      (val (read input-file))
                      (navcom (list dir val)))
                 (if (eof-object? dir)
                     ls
                     (R (cons navcom ls)))))))
    (reverse (R '()))))

(define move-simple
  (lambda (navcom curpos)
    (record-case navcom
      ((forward) (val) (list (+ (car curpos) val) (cadr curpos)))
      ((up)      (val) (list (car curpos) (- (cadr curpos) val)))
      ((down)    (val) (list (car curpos) (+ (cadr curpos) val)))
      (else (assertion-violationf 'navigate "invalid expression ~s" navcom)))))

(define navigate
  (lambda (move navcoms)
    (letrec ((N (lambda (navcoms curpos)
                  (if (null? navcoms)
                      curpos
                      (N (cdr navcoms) (move (car navcoms) curpos))))))
      (N navcoms '(0 0 0)))))

(define solution
  (lambda (movef navcoms)
    (let ((finalpos (navigate movef navcoms)))
      (* (car finalpos) (cadr finalpos)))))

(define solution-part1 (solution move-simple input))

;;; Part 2
;;; ------
(define move-aim
  (lambda (navcom curpos)
    (let ((horizontal (car curpos))
          (depth (cadr curpos))
          (aim (caddr curpos)))
      (record-case navcom
        ((forward) (val) (list (+ horizontal val) (+ depth (* aim val)) aim))
        ((up)      (val) (list horizontal depth (- aim val)))
        ((down)    (val) (list horizontal depth (+ aim val)))
        (else (assertion-violationf 'navigate "invalid expression ~s" navcom))))))

(define solution-part2 (solution move-aim input))

(display (format "Part 1: ~s~%Part 2: ~s"
                 solution-part1
                 solution-part2))