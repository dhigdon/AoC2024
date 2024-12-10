;;; Advent of Code 2024, Day 10 - Hoof It
;;; by Dan Higdon

;; 'pos' is the current position on the map grid
(defstruct pos (x 0 :type integer) (y 0 :type integer))
(defun pos-up (pos)     (make-pos :x (pos-x pos) :y (1- (pos-y pos))))
(defun pos-down (pos)   (make-pos :x (pos-x pos) :y (1+ (pos-y pos))))
(defun pos-left (pos)   (make-pos :x (1- (pos-x pos)) :y (pos-y pos)))
(defun pos-right (pos)  (make-pos :x (1+ (pos-x pos)) :y (pos-y pos)))

(defun char-height (ch) (- (char-code ch) (char-code #\0)))

(defun make-map (width height)
  (make-array (list height width) :element-type 'integer :initial-element 0))

(defun map-height (pos map)
  (let ((x (pos-x pos))
        (y (pos-y pos)))
    (if (and (< -1 y (array-dimension map 0))
             (< -1 x (array-dimension map 1)))
      (aref map y x)
      -1)))

(defun find-trailheads (map)
  (let (result)
    (dotimes (y (array-dimension map 0))
      (dotimes (x (array-dimension map 1))
        (when (zerop (aref map y x))
          (push (make-pos :x x :y y) result))))
    result))

(defun find-adjacent (pos map)
  (let ((ht (1+ (map-height pos map)))
        (up (pos-up pos))
        (dn (pos-down pos))
        (lf (pos-left pos))
        (rt (pos-right pos)))
    (concatenate 'list
                 (when (= (map-height up map) ht) (list up))
                 (when (= (map-height dn map) ht) (list dn))
                 (when (= (map-height lf map) ht) (list lf))
                 (when (= (map-height rt map) ht) (list rt)))))

(defun read-map (fname)
  (with-open-file (file (pathname fname))
    (let* ((size (length (read-line file)))
           (map (make-map size size)))
      (file-position file :start)
      (dotimes (y size map)
        (dotimes (x size)
          (setf (aref map y x) (char-height (read-char file))))
        (read-char file)))))

(defun advance-trailheads (heads map &optional (test #'equalp))
  (reduce (lambda (a b) (union a b :test test))
          (mapcar (lambda (p) (find-adjacent p map)) heads)))

(defun walk-trail (head map &optional (test #'equalp))
  "Walk one trail to completion, and return the number of trails"
  (let ((pos (list head)))
    (dotimes (i 9)
      (setf pos (advance-trailheads pos map test)))
    (length pos)))

(defun part1 (fname)
  (let* ((m (read-map fname))
         (hs (find-trailheads m)))
    (apply #'+ (mapcar (lambda (h) (walk-trail h m)) hs))))

(defun part2 (fname)
  (let* ((m (read-map fname))
         (hs (find-trailheads m)))
    (apply #'+ (mapcar (lambda (h) (walk-trail h m #'eq)) hs))))

