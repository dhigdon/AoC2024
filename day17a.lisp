;;; Advent of Code 2024, Day 17 - 

(defun combo (opcode a b c)
  (case opcode
    ((0 1 2 3) opcode)
    (4 a)
    (5 b)
    (6 c)))

(defun run (inst ip a b c)
  (loop
    while (< ip (length inst))
    do (let ((operand (svref inst (1+ ip))))
         (case (svref inst ip) 
           (0 (setf a (floor a (expt 2 (combo operand a b c))))
            (incf ip 2))

           (1 (setf b (logxor operand b)) 
            (incf ip 2))

           (2 (setf b (mod (combo operand a b c) 8))
            (incf ip 2))

           (3 (if (zerop a)
                (incf ip 2)
                (setf ip operand)))

           (4 (setf b (logxor b c))
            (incf ip 2))

           (5 (format t "~A," (mod (combo operand a b c) 8))
            (incf ip 2))

           (6 (setf b (floor a (expt 2 (combo operand a b c))))
            (incf ip 2))


           (7 (setf c (floor a (expt 2 (combo operand a b c))))
            (incf ip 2))))))

(defun test ()
  (run #(0 1 5 4 3 0) 0 729 0 0))

;; Problem Data
;; Register A: 51342988
;; Register B: 0
;; Register C: 0
;; Program: 2,4,1,3,7,5,4,0,1,3,0,3,5,5,3,0
;; Disassembly:
;;    BST B=A MOD 8
;;    BXL B^=3
;;    CDV C=A/B
;;    BXC B=B^C
;;    BXL B^=3
;;    ADV A=A/8
;;    OUT B
;;    JNZ 0

(defun part1 ()
  (run #(2 4 1 3 7 5 4 0 1 3 0 3 5 5 3 0) 0 51342988 0 0))

