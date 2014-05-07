(defvar test-eval-test-bodies nil)

(defmacro unit-tests (&rest body)
  "Declare a series of unit tests.
The body of a `unit-tests' form is elisp code that calls test functions.
Normally, it is ignored, but `test-eval-buffer' evaluates the buffer and
evaluates the `unit-tests' form bodies.

Any function that signals a test-failed condition on failure can
be a test function, but if you use the functions defined in this
file, when a test fails, they will leave point at the failing
test."
  (declare (indent defun))
  (if (not test-eval-test-bodies)
      nil

    ;; If standard-input is a buffer or marker, then we can actually make
    ;; failures point at the failing test. Otherwise, we just print a
    ;; message.
    ;;
    ;; We do this by *re-parsing* the body of the `unit-tests' form, and then
    ;; expanding to a progn whose body is a series of `let' expressions
    ;; that bind `test-buffer' and `test-point' to the buffer name and starting
    ;; position of that form.
    (let ((stream standard-input))
      (when (bufferp stream)
        (setq stream (save-excursion
                       (set-buffer stream)
                       (point-marker))))
      (if (not (markerp stream))
          ;; Not a buffer; give up on providing source positions, but do
          ;; run the tests.
          ;;
          ;; Issue: This doesn't bind the dynamic variables, so the
          ;; signalling and handling won't work right.
          `(progn ,@body)

        (let (forms)
          (save-excursion
            (set-buffer (marker-buffer stream))
            ;; Point is currently after the (test ...) form.
            ;; Place point after the first sexp in the list, the `unit-tests' symbol.
            (goto-char (scan-sexps (marker-position stream) -1))
            (goto-char (scan-lists (point) 1 -1))
            (unless (eq (read (current-buffer)) 'unit-tests)
              (error "Don't seem to be able to find the (unit-tests ...) form"))

            ;; Find the end of the next sexp, while there is such to be found.
            (while (condition-case nil
                       (goto-char (scan-sexps (point) 1))
                     (scan-error nil))
              ;; Back up to the beginning of that sexp.
              (goto-char (scan-sexps (point) -1))
              (let ((start-pos (point)))
                (push `(let ((test-point ,start-pos))
                         ,(read (current-buffer)))
                      forms)))

            `(let ((test-buffer ,(buffer-name)))
               ,@(reverse forms))))))))

(defmacro test-leave-point-at-failure (&rest body)
  (declare (indent 0))
  `(let ((test-eval-test-bodies t))
     (condition-case desc
         (progn
           (progn ,@body)
           (message "All tests executed without failure."))
       (test-failed
        (message "%s" (error-message-string desc))
        (switch-to-buffer (nth 2 desc))
        (goto-char (nth 3 desc))))))

(defun test-eval-buffer ()
  "Evaluate the current buffer, and run tests appearing in `unit-tests' bodies."
  (interactive)
  (test-leave-point-at-failure
    (eval-buffer)))

;; eval-region doesn't seem to work the way we're expecting...
;; (defun test-eval-region (start end)
;;  "Evaluate the current region, and run tests appearing in `unit-tests' bodies."
;;  (interactive "r")
;;  (test-leave-point-at-failure
;;    (eval-region start end)))

(put 'test-failed 'error-conditions '(error test-failed))
(put 'test-failed 'error-message "Unit test failed")

(defun test-fail (message)
  (signal 'test-failed (list message test-buffer test-point)))

(defun test-equal (actual expected)
  (unless (equal actual expected)
    (test-fail (format "Expected %S, got %S" expected actual))))

(defmacro test-matches (actual pattern)
  (declare (indent defun))
  `(pcase ,actual
     (,pattern t)
     (_ (test-fail (format "Result %S does not match pattern %S"
                           actual ',pattern)))))

(defmacro test-with-input (expr)
  "Evaluate expr with point at the start of the comment text immediately following the `test-parse' form."
  `(save-excursion
     (goto-char (scan-sexps test-point 1))
     (skip-chars-forward " \t\n;")
     (let ((standard-input (current-buffer)))
       ,expr)))
