
(defun replicate (item times)
  (make-list times :initial-element item))

(defun char-value (ch)
  (- (char-code ch) (char-code #\0)))

(defun parsemap (line)
  "Given a list of run lengths alternating between files and
   unused space, generate the block map. Each file's id
   is sequential, and blank space effectively has an id of -1"
  (do* ((n 0 (1+ n))
        (id 0 (if (oddp n) id (1+ id)))
        (r (replicate (if (oddp n) -1 id) 
                      (char-value (char line n))) 
           (nconc r
             (replicate (if (oddp n) -1 id) 
                        (char-value (char line n))))))
    ((= (1+ n) (length line))
     (apply #'vector r))))

(defun readmap (fname)
  (with-open-file (f (pathname fname))
    (parsemap (string-right-trim #(#\Return #\Linefeed) (read-line f)))))

(defvar testdata (readmap "day9_test.txt"))

(defun checksum (data)
  (loop for i from 0 to (1- (length data))
        unless (= -1 (aref data i))
        summing (* i (aref data i))))


;; Part 1
(defun next-space (data start)
  (cond ((>= start (length data)) (length data))
        ((= (aref data start) -1) start)
        (t (next-space data (1+ start)))))

(defun prev-data (data start)
  (cond ((<= start 0) nil)
        ((> (aref data start) -1) start)
        (t (prev-data data (1- start)))))

(defun part1 (data)
  (do* ((left (next-space data 0)
              (next-space data left))
        (right (prev-data data (1- (length data)))
               (prev-data data right)))
    ((or (not left) (not right) (> left right)) (checksum data))
    (setf (aref data left) (aref data right))
    (setf (aref data right) -1)))


;; Part 2 - not pretty or apparently very fast, but it does work

(defstruct file
  id
  (start 0 :type integer)
  (len 0 :type integer))

(defun file-end (file)
  (+ (file-start file) (file-len file)))


(defun block-start (data start val)
  "Returns the index of the first occurrence of 'val' before 'start'.
  Assumes that 'start' is currently on an index of 'data' that has 'val'."
  (declare (fixnum start val))
  (cond ((<= start 0) 0)
        ((/= (aref data start) val) (1+ start))
        (t (block-start data (1- start) val))))

(defun find-run (data start key)
  "Return the index after a run of 'key' values, starting at 'start'"
  (declare (fixnum start key))
  (cond ((>= start (length data)) (length data))
        ((/= key (aref data start)) start)
        (t (find-run data (1+ start) key))))

(defun find-hole (data start length)
  "Return the index of a hole after 'start' large enough
  to hold 'length', or nil if none is found."
  (declare (fixnum start length))
  (let* ((s (next-space data start))
         (e (find-run data s -1))
         (l (- e s)))
    (declare (fixnum s e l))
    (cond ((<= l 0)        nil)
          ((>= l length)   s)
          (t (find-hole data e length)))))

(defun find-last (data &optional (idx (1- (length data))))
  (declare (fixnum idx))
  (cond ((< idx 0) nil)
        ((/= -1 (aref data idx)) idx)
        (t (find-last data (1- idx)))))
                                 
(defun find-last-file (data pos)
  "Returns the last file before 'pos' in the data"
  (declare (fixnum pos))
  (let ((end (find-last data pos)))
    (when end
      (let* ((id (aref data end))
             (start (block-start data end id)))
        (make-file :id id :start start :len (- end start -1))))))

(defun erase-file (data file)
  (fill data -1 :start (file-start file) :end (file-end file))
  file)

(defun save-file (data file)
  (fill data (file-id file) :start (file-start file) :end (file-end file))
  file)

(defun part2 (data &optional (pos (1- (length data))))
  (let ((f (find-last-file data pos)))
    (cond ((null f) (checksum data))
          (t (let ((hole (find-hole data 0 (file-len f))))
               (when (and hole (< hole (file-start f)))
                 (erase-file data f)
                 (save-file data 
                            (make-file :id (file-id f)
                                       :start hole
                                       :len (file-len f)))))
             (part2 data (1- (file-start f)))))))

