;;; instruction representation
;;;
;;; operand sizes (Z):
;;;   word
;;;   byte
;;;
;;; register (R):
;;;   0..15
;;;
;;; condition register bits affected (CC)
;;;   nil                      no effect
;;;   +                        set as for 'add'-like instructions
;;;   -                        set as for 'sub'-like instructions
;;;
;;;   Z: (z? E)
;;;   N: (n? E)
;;;   C: (c? E1 E2)            For 'add'-like instructions
;;;      (>=? E1 E2)           For 'sub'-like instructions
;;;
;;; operands (E): no side effects, just values; all use arithmetic modulo 2^16
;;;
;;;   (+ E1 E2)                addition
;;;   (- E)                    negation
;;;   (& E1 E2)                logical and
;;;   (| E1 E2)                logical or
;;;   (fetch Z E)              memory fetch
;;;   (reg R)                  register reference
;;;   K                        constant integer
;;;   (z? E)                   1 if E is zero, else 0
;;;   (c? E1 E2)               1 if E1 + E2 >= 2^16 using full arithmetic, else 0
;;;   (>=? E1 E2)              1 if E1 >= E2
;;;   (n? E)                   1 if E1's top (2^15) bit is set, else 0
;;;
;;; statements (S): can have side effects
;;;
;;;   (set R E)                set register R to E; set flags according to CC
;;;   (test CC E)              set flags for E according to CC
;;;   (store Z E1 E2)          store E2 as a value of size Z at address E1
;;;   (jump E)                 jump to address E unconditionally
;;;   (branch C E)             jump to address E if (C R2 0)
;;;   (other label ADDR NAME)       assembly label
;;;   (other section ADDR NAME)     section name
;;;   (other string ADDR TEXT)      string literal

(require 'cl)

(defun comma-list (p)
  "Return a list of comma-separated items parsed by p.
If P recognizes the text at point, it should return its parsed
form, leaving point after the text. Otherwise, P should return
nil and leave point unmoved."
  (let ((v (funcall p))
        l)
    (when v
      (push v l)
      (while (looking-at "[ \t]*,[ \t]*")
        (goto-char (match-end 0))
        (setq v (funcall p))
        (unless v (error "expected another 'p' after comma"))
        (push v l)))
    (reverse l)))

(defconst const-regexp "\\(\\(-\\)?0x\\([0-9a-f]+\\)\\)")
(defun parse-const (c)
  (save-match-data
    (unless (string-match const-regexp c)
      (error "bad constant"))
    (* (string-to-number (match-string 3 c) 16)
       (if (match-string 2 c) -1 1))))

(unit-tests
  (test-equal (parse-const "0x0") 0)
  (test-equal (parse-const "0x1") 1)
  (test-equal (parse-const "0xfff") 4095)
  (test-equal (parse-const "-0x5") -5)
  (test-equal (parse-const "-0x1") -1)
  )

(defconst register-regexp "\\(r1[0-5]\\|r[0-9]\\|sp\\|pc\\|sr\\)")
(defun parse-register (r)
  (save-match-data
    (cond
     ((string-match "\\`r\\([0-9]+\\)\\'" r)
      (list 'reg (string-to-number (match-string 1 r))))
     ((string= r "pc") '(reg 0))
     ((string= r "sp") '(reg 1))
     ((string= r "sr") '(reg 2))
     (t (error "unrecognized register")))))

(defun parse-operand ()
  (let ((v (cond
            ((looking-at register-regexp)
             (parse-register (match-string 1)))
            ((looking-at (concat "#" const-regexp "\\( <[^>]+>\\)?"))
             (parse-const (match-string 1)))
            ((looking-at (concat "&" const-regexp))
             `(fetch ,parse-size ,(parse-const (match-string 1))))
            ((looking-at (concat const-regexp "(" register-regexp ")"))
             `(fetch ,parse-size ,(expr-+ (parse-const (match-string 1))
                                          (parse-register (match-string 4)))))
            (t nil))))
    (when v (goto-char (match-end 0)))
    v))

(defun parse ()
  "Parse Microcorruption-style MSP430 disassembly at point.
Return a form (MARKER ADDR (S ...))."
  (let ((m (point-marker)))
    (cond
     ((looking-at "\\([0-9a-f]+\\):  \\(?:[0-9a-f]+ \\)+ *\\([a-z.]+\\)+")
      (let ((addr (string-to-number (match-string 1) 16)))
        (goto-char (match-beginning 2))
        (let ((stmts (parse-insn)))
          (list m addr stmts))))
     ((looking-at "\\([0-9a-f]+\\) <\\(.*\\)>")
      (prog1 (list m (match-string-no-properties 1) 'other 'label (match-string-no-properties 2))
        (forward-line 1)))
     ((looking-at "\\([0-9a-f]+\\) \\.\\([^:]*\\):")
      (prog1 (list m (match-string-no-properties 1) 'other 'section (match-string-no-properties 2))
        (forward-line 1)))
     ((looking-at "\\([0-9a-f]+\\): \"\\([^:]*\\)\"")
      (prog1 (list m (match-string-no-properties 1) 'other 'string (match-string-no-properties 2))
        (forward-line 1)))
     (t nil))))

(defconst opcode-table
  '((and (source dest) arithmetic & +)
    (add (source dest) arithmetic + +)
    (bis (source dest) arithmetic | nil)
    (sub (source dest) arithmetic - -)
    (mov (source dest) move)
    (ret () misc)
    (clr (dest) misc)))

(defun parse-insn ()
  "Parse the MSP430 instruction at point.
Return a list of statements that have the same effect as the instruction."
  (unless (looking-at "\\([a-z]+\\)\\(\\.b\\)?[ \t]*")
    (error "malformed opcode"))
  (let ((opcode (intern (match-string-no-properties 1)))
        (byte-suffix (match-end 2)))
    (goto-char (match-end 0))
    (let* ((parse-size (if byte-suffix 'byte 'word))
           (opns (comma-list 'parse-operand)))
      (unless (eolp) (error "junk at end of line"))
      (forward-line 1)
      (let ((def (assq opcode opcode-table)))
        (unless def (error "unrecognized opcode"))
        (unless (= (length opns) (length (cadr def))) (error "wrong number of operands"))
        (cl-mapc (lambda (opnt opn)
                   (case opnt
                     ((dest)
                      (unless (and (consp opn) (memq (car opn) '(reg fetch)))
                        (error "destination operations must be 'reg' or 'fetch'")))
                     ((const)
                      (unless (numberp opn)
                        (error "operand must be a constant")))
                     ((reg)
                      (unless (and (consp opn) (eq (car opn) 'reg))
                        (error "operand must be a register")))))
                 (cadr def) opns)
        (case (caddr def)
          ((arithmetic)
           (let ((src (if byte-suffix (expr-force-byte (car opns)) (car opns)))
                 (dst (cadr opns))
                 (cc  (nth 4 def)))
             (let* ((val (expr-any (cadddr def) dst src))
                    (write (list (stmt-write dst val))))
               (if cc (cons `(test ,cc ,val) write)
                 write))))
          ((move)
           (let ((src (if byte-suffix (expr-force-byte (car opns)) (car opns)))
                 (dst (cadr opns)))
             (list (stmt-write dst src))))
          ((misc)
           (case opcode
             ((ret)
              '((set 0 (fetch word (reg 1)))
                (set 1 (+ 2 (reg 1)))))
             ((clr)
              (list (stmt-write (car opns) 0)))))
          (otherwise (error "bogus instruction type in opcode-table")))))))

(unit-tests
  (test-with-input (test-equal (parse-insn) '((store byte (+ 9216 (reg 15)) 0))))
  ;; mov.b	#0x0, 0x2400(r15)

  (test-with-input (test-equal (parse-insn) '((store byte (+ 36 (reg 11)) (& 255 (reg 10))))))
  ;; mov.b   r10, 0x24(r11)

  (test-with-input (test-equal (parse-insn) '((store byte (reg 11) (fetch byte (reg 10))))))
  ;; mov.b   0x0(r10), 0x0(r11)

  (test-with-input (test-equal (parse-insn) '((store word (reg 11)  (fetch word (reg 10))))))
  ;; mov   0x0(r10), 0x0(r11)

  (test-with-input (test-equal (parse-insn) '((set 1 17408))))
  ;; mov	#0x4400, sp

  (test-with-input (test-equal (parse-insn) '((set 5 255))))
  ;; mov.b	#-0x1, r5

  (test-with-input (test-equal (parse-insn) '((test + (& 255 (reg 5))) (set 5 (& 255 (reg 5))))))
  ;; and.b	#-0x1, r5

  (test-with-input (test-equal (parse-insn) '((test + (& 255 (reg 5))) (set 5 (& 255 (reg 5))))))
  ;; and.b   #0xff, r5

  (test-with-input (test-equal (parse-insn) '((set 5 (| 23048 (reg 5))))))
  ;; bis	#0x5a08, r5

  (test-with-input (test-equal (parse-insn)
                               '((set 0 (fetch word (reg 1)))
                                 (set 1 (+ 2 (reg 1))))))
  ;; ret

  (test-with-input (test-equal (parse-insn) '((set 15 0))))
  ;; clr r15
)

(defun bytep (n)
  (and (numberp n) (<= 0 n) (<= n 255)))

(defun expr-bytep (e)
  (pcase e
    ((pred bytep) t)
    (`(fetch byte ,_) t)
    (`(& ,a ,b) (or (expr-bytep a) (expr-bytep b)))
    (_ nil)))

(defun expr-& (a b)
  (cond
   ((and (numberp a) (numberp b))
    (logand a b))
   ((numberp b)
    (expr-& b a))
   ((and (numberp a) (zerop a)) 0)
   ((and (numberp a) (= (logand a #xffff) #xffff))
    b)
   ((and (numberp a) (= (logand a #xff) #xff) (expr-bytep b))
    b)
   (t (pcase `(& ,a ,b)
        ((and `(& ,x (& ,y ,z))
              (guard (and (numberp x) (numberp y))))
         (expr-& (logand x y) z))
        (e e)))))

(unit-tests
  (test-equal (expr-& 4 5) 4)
  (test-equal (expr-& 'x 5) '(& 5 x))
  (test-equal (expr-& 'x 0) 0)
  (test-equal (expr-& 'x -1) 'x)
  (test-equal (expr-& '(fetch byte 0) 255) '(fetch byte 0))
  (test-equal (expr-& 7 '(& 3 x)) '(& 3 x))
  (test-equal (expr-& '(& 3 x) 7) '(& 3 x))
  (test-equal (expr-& 'x 'y) '(& x y))
  )

(defun expr-force-byte (e)
  (expr-& #xff e))

(unit-tests
  (test-equal (expr-force-byte 0) 0)
  (test-equal (expr-force-byte 255) 255)
  (test-equal (expr-force-byte 256) 0)
  (test-equal (expr-force-byte '(reg 4)) '(& 255 (reg 4)))
  (test-equal (expr-force-byte '(fetch word 42)) '(& 255 (fetch word 42)))
  (test-equal (expr-force-byte '(fetch byte 42)) '(fetch byte 42))
  (test-equal (expr-force-byte '(& (fetch byte 42) 1023)) '(& (fetch byte 42) 1023))
  (test-equal (expr-force-byte '(& (fetch word 42) 15)) '(& (fetch word 42) 15))
  )

(defun expr-| (a b)
  (cond
   ((and (numberp a) (numberp b))
    (logior a b))
   ((numberp b)
    (expr-| b a))
   ((and (numberp a) (zerop a))
    b)
   ((and (numberp a) (= (logand a #xffff) #xffff))
    #xffff)
   ((and (numberp a) (= (logand a #xff) #xff) (expr-bytep b))
    #xff)
   (t (pcase `(| ,a ,b)
        ((and `(| ,x (| ,y ,z))
              (guard (and (numberp x) (numberp y))))
         (expr-| (logior x y) z))
        (e e)))))

(unit-tests
  (test-equal (expr-| 10 5) 15)
  (test-equal (expr-| 'x 5) '(| 5 x))
  (test-equal (expr-| 'x 0) 'x)
  (test-equal (expr-| 'x #xffff) 65535)
  (test-equal (expr-| '(fetch byte x) #xff) 255)
  (test-equal (expr-| '(| 8 x) 16) '(| 24 x))
  (test-equal (expr-| 'x 'y) '(| x y))
  )

(defun expr-+ (a b)
  (cond
   ((and (numberp a) (numberp b))
    (logand #xffff (+ a b)))
   ((numberp b)
    (expr-+ b a))
   ((and (numberp a) (zerop a)) b)
   (t (pcase `(+ ,a ,b)
        ((and `(+ ,x (+ ,y ,z))
              (guard (and (numberp x) (numberp y))))
         (expr-+ (logand #xffff (+ x y)) z))
        (`(+ (- ,x) (- ,y)) (expr-neg (expr-+ x y)))
        (e e)))))

(unit-tests
  (test-equal (expr-+ 10 20) 30)
  (test-equal (expr-+ 'x 20) '(+ 20 x))
  (test-equal (expr-+ 'x 0) 'x)
  (test-equal (expr-+ '(+ 20 x) 10) '(+ 30 x))
  (test-equal (expr-+ 'x 'y) '(+ x y))
  (test-equal (expr-+ '(- x) '(- y)) '(- (+ x y)))
  )

(defun expr-neg (a)
  (cond
   ((numberp a)
    (logand #xffff (- a)))
   (t (pcase `(- ,a)
        (`(- (- ,e)) e)
        (`(- (+ ,x (- ,y))) (expr-+ y (expr-neg x)))
        (e e)))))

(unit-tests
  (test-equal (expr-neg 1) 65535)
  (test-equal (expr-neg '(- x)) 'x)
  (test-equal (expr-neg 'x) '(- x))
  )

(defun expr-- (a b) (expr-+ a (expr-neg b)))

(unit-tests
  (test-equal (expr-- 10 20)  65526)
  (test-equal (expr-- 'x 20) '(+ 65516 x))
  (test-equal (expr-- 'x 0) 'x)
  (test-equal (expr-- 0 'x) '(- x))
  (test-equal (expr-- 10 'x) '(+ 10 (- x)))
  (test-equal (expr-- 10 (expr-- 10 'x)) 'x)
  (test-equal (expr-- 12 (expr-- 10 'x)) '(+ 2 x))
  )

(defun expr-any (op a b)
  (case op
    ((+) (expr-+ a b))
    ((-) (expr-- a b))
    ((&) (expr-& a b))
    ((|) (expr-| a b))
    (otherwise (list op a b))))

(defun stmt-write (dst src)
  (pcase dst
    (`(reg ,r) `(set ,r ,src))
    (`(fetch ,z ,e) `(store ,z ,e ,src))
    (_ (error "bad dst"))))
