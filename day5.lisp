;;; Advent of Code 2024, Day 5 - Print Queue
;;; by Dan Higdon

(defstruct node id successors)

(defun push-successor (successor node)
  (push successor (node-successors node)))

(defstruct graph nodes)

(defun find-node (id graph)
  (gethash id graph))

(defun add-edge (pair graph)
  "Adds the node if not present, and adds the sucessor connection"
  (let ((id (car pair))
        (succ (cdr pair)))
    (push-successor succ
                    (or (gethash id graph)
                        (setf (gethash id graph) (make-node :id id))))))

(defun read-datafile (fname)
  (let ((graph (make-hash-table))
        (runs (list)))
    (with-open-file (file (pathname fname))
      (loop for v = (read file)
            until (eq v :data)
            do (add-edge v graph))
      (loop for v = (read file nil)
            until (null v)
            do (push v runs)))
    (values graph runs)))


(defun find-graph-node (start target graph)
  (cond ((eq start target) target)
        ((find-node start graph)
         (find-if (lambda (x) (find-graph-node x target graph))
                  (node-successors (find-node start graph))))
        (t nil)))

(defun find-graph-node2 (start target graph)
  (cond ((eq start target) target)
        ((find-node start graph)
         (member-if (lambda (x) (find-graph-node x target graph))
                    (node-successors (find-node start graph))))
        (t nil)))

(defun valid-sequence (seq graph)
  (cond ((numberp seq) t)
        ((consp seq)
         (and (find-graph-node (car seq) (cadr seq) graph)
              (valid-sequence (cdr seq) graph)))
        (t nil)))

;; Part 1 - see if all the pages in a run are consecutive 
;; according to the graph.


