;;; Advent of Code 2024, Day 17 - 

(defun combo (opcode a b c)
  (case opcode
    ((0 1 2 3) opcode)
    (4 a)
    (5 b)
    (6 c)))

(defun run (inst ip a b c)
  (when (< ip (length inst))
    (let ((opcode (svref inst ip))
          (operand (svref inst (1+ ip))))
      (case opcode
        (0 (run inst (+ 2 ip) (floor a (expt 2 (combo operand a b c))) b c))
        (1 (run inst (+ 2 ip) a (logxor operand b) c))
        (2 (run inst (+ 2 ip) a (mod (combo operand a b c) 8) c))
        (3 (if (zerop a)
             (run inst (+ 2 ip) a b c)
             (run inst operand a b c)))
        (4 (run inst (+ 2 ip) a (logxor b c) c))
        (5 (format t "~A," (mod (combo operand a b c) 8))
         (run inst (+ 2 ip) a b c))
        (6 (run inst (+ 2 ip) a (floor a (expt 2 (combo operand a b c))) c))
        (7 (run inst (+ 2 ip) a b (floor a (expt 2 (combo operand a b c)))))))))

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

