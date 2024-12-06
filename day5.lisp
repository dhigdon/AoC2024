;;; Advent of Code 2024, Day 5 - Print Queue
;;; by Dan Higdon

(defun read-datafile (fname)
  "Read the pre-conditioned data file, yielding the rules and the books"
  (with-open-file (file (pathname fname))
    (values
      ; Rules
      (loop for v = (read file)
            until (eq v :data)
            collecting v)
      ; Books
      (loop for v = (read file nil)
            until (null v)
            collecting v))))

(defun check-rule (a b rules)
  "Check that b follows a in the rules"
  (member (cons a b) rules :test #'equal))

(defun validate (pages rules)
  "Validate that pages are in the order defined by the rules"
  (reduce (lambda (l r)
            (and l (when (check-rule l r rules) r)))
          pages))

(defun center (l)
  "Return the center element of l"
  (nth (round (/ (1- (length l)) 2)) l))

(defun part1a (filename)
  "Solution to part 1 - determine which books are sorted"
  (multiple-value-bind (rules books) (read-datafile filename)
    (loop for b in books
          when (validate b rules)
          summing (center b))))

(defun part2 (filename)
  "Solution to part 2 - repair the unsorted books"
  (multiple-value-bind (rules books) (read-datafile filename)
    (loop for b in books
          unless (validate b rules)
          summing (center (sort b (lambda (l r) (check-rule r l rules)))))))

