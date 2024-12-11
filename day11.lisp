;;; Advent of Code 2024, Day 11 - Plutonian Pebbles
;;; by Dan Higdon

(defparameter *test* '(125 17))
(defparameter *data* '(1 24596 0 740994 60 803 8918 9405859))

(defun blink (blinks pebble cache)
  "Compute the length of the pebble segment a given pebble generates
  after 'blink' blinks. Memoize the results in 'cache' for performance."
  (let ((key (cons blinks pebble))
        (next (1- blinks)))
    (or (gethash key cache)
        (setf
          (gethash key cache)
          (cond ((zerop blinks) 1)
                ((zerop pebble) (blink next 1 cache))
                (t (let* ((s (write-to-string pebble))
                          (len (length s)))
                     (if (evenp len)
                       (+ (blink next (parse-integer s :end   (/ len 2)) cache)
                          (blink next (parse-integer s :start (/ len 2)) cache))
                       (blink next (* pebble 2024) cache)))))))))

(defun run (data blinks)
  (loop with cache = (make-hash-table :test #'equal)
        for i in data
        summing (blink blinks i cache)))

(defun part1 (data) (run data 25))
(defun part2 (data) (run data 75))
