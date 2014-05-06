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
;;;   (set CC R E)             set register R to E; set flags according to CC
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
;; (parse-const "")
;; (parse-const "0x1") 1
;; (parse-const "0xfff") 4095
;; (parse-const "-0x5") -5
;; (parse-const "-0x1") -1

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
             `(fetch ,parse-size (+ ,(parse-const (match-string 1))
                                    ,(parse-register (match-string 4)))))
            (t nil))))
    (when v (goto-char (match-end 0)))
    v))

(defconst opcode-table
  '((mov move (source dest))
    (ret return ())))

(defun parse ()
  "Parse Microcorruption-style MSP430 disassembly at point.
Return a form (MARKER ADDR (S ...))."
  (let ((m (point-marker)))
    (cond
     ((looking-at "\\([0-9a-f]+\\) <\\(.*\\)>")
      (prog1 (list m (match-string-no-properties 1) 'other 'label (match-string-no-properties 2))
        (forward-line 1)))
     ((looking-at "\\([0-9a-f]+\\) \\.\\([^:]*\\):")
      (prog1 (list m (match-string-no-properties 1) 'other 'section (match-string-no-properties 2))
        (forward-line 1)))
     ((looking-at "\\([0-9a-f]+\\): \"\\([^:]*\\)\"")
      (prog1 (list m (match-string-no-properties 1) 'other 'string (match-string-no-properties 2))
        (forward-line 1)))

     ;; Real instructions.
     ((looking-at "\\([0-9a-f]+\\):  \\(?:[0-9a-f]+ \\)+ *\\([a-z]+\\(\\.b\\)?\\)[ \t]*")
      (let ((addr (string-to-number (match-string 1) 16))
            (opcode (intern (match-string-no-properties 2)))
            (byte-suffix (match-end 3)))
        (goto-char (match-end 0))
        (let* ((parse-size (if byte-suffix 'byte 'word))
               (opns (comma-list 'parse-operand)))
          (unless (eolp) (error "junk at end of line"))
          (forward-line 1)
          (let ((def (assq opcode opcode-table)))
            (unless def (error "unrecognized opcode"))
            (unless (= (length opns) (length (caddr def))) (error "wrong number of operands"))
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
                     (caddr def) opns)
            (let ((stmts
                   (case (cadr def)
                     ((move)
                      (pcase opns
                        (`(,src (reg ,r)) `((set nil ,r ,src)))
                        (`(,src (fetch ,z ,e)) `((store ,z ,e ,src)))
                        (_ (error "bad move opnd check"))))
                     ((return)
                      '((set nil 0 (fetch word (reg 1)))
                        (set nil 1 (+ 2 (reg 1)))))
                     (otherwise (error "bogus instruction type in opcode-table")))))
              (list m addr stmts))))))

     (t nil))))

