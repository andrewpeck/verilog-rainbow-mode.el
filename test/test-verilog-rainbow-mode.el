;;; test-verilog-rainbow-mode.el -*- lexical-binding: t; -*-

(require 'verilog-rainbow-mode)
(require 'ert)

;;; Helpers

;; Declare as special so (let ((rainbow-delimiters-pick-face-function ...)))
;; creates a dynamic binding visible inside verilog-rainbow-mode--apply-color-range.
(defvar rainbow-delimiters-pick-face-function)

(defun verilog-rainbow-test--pick-face (depth _match _start)
  "Return test-face-N for DEPTH > 0, nil otherwise."
  (when (> depth 0)
    (intern (format "test-face-%d" depth))))

(defmacro verilog-rainbow-test--with-buffer (content &rest body)
  "Evaluate BODY in a temp buffer with CONTENT, the mode enabled."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     ;; Set the minor-mode variable directly (without running the full
     ;; define-minor-mode body, which would invoke font-lock-add-keywords
     ;; and font-lock-flush on a buffer with no font-lock setup).
     (setq-local verilog-rainbow-mode t)
     (let ((rainbow-delimiters-pick-face-function
            #'verilog-rainbow-test--pick-face))
       ,@body)))

(defun verilog-rainbow-test--face-at (pos)
  "Return the font-lock-face text property at POS as a list.
font-lock-prepend-text-property always stores a list in Emacs 29+."
  (let ((face (get-text-property pos 'font-lock-face)))
    (if (listp face) face (list face))))

(defun verilog-rainbow-test--has-face-p (pos face)
  "Return non-nil if FACE is among the font-lock faces at POS."
  (member face (verilog-rainbow-test--face-at pos)))

(defun verilog-rainbow-test--depth-at (pos)
  "Return the numeric depth encoded in the test face at POS, or nil."
  (let ((face (car (verilog-rainbow-test--face-at pos))))
    (when (and face (string-match "test-face-\\([0-9]+\\)" (symbol-name face)))
      (string-to-number (match-string 1 (symbol-name face))))))

(defun verilog-rainbow-test--keyword-depths ()
  "Return a list of (KEYWORD DEPTH) for each begin/end in the current buffer."
  (let (result)
    (goto-char (point-min))
    (while (re-search-forward "\\_<begin\\_>\\|\\_<end\\_>" nil t)
      (push (list (match-string 0)
                  (verilog-rainbow-test--depth-at (match-beginning 0)))
            result))
    (nreverse result)))

;;; Tests: propertize function

(ert-deftest verilog-rainbow-test--single-pair ()
  "Single begin/end pair is colored at depth 1."
  ;; Buffer: "begin end"
  ;;          123456789
  (verilog-rainbow-test--with-buffer "begin end"
    (verilog-rainbow-mode--propertize-verilog nil)
    (should (verilog-rainbow-test--has-face-p 1 'test-face-1))
    (should (verilog-rainbow-test--has-face-p 7 'test-face-1))))

(ert-deftest verilog-rainbow-test--nested-pairs ()
  "Nested begin/end pairs use increasing depths."
  ;; Buffer: "begin begin end end"
  ;;          1     7    13  17
  (verilog-rainbow-test--with-buffer "begin begin end end"
    (verilog-rainbow-mode--propertize-verilog nil)
    (should (verilog-rainbow-test--has-face-p 1  'test-face-1))
    (should (verilog-rainbow-test--has-face-p 7  'test-face-2))
    (should (verilog-rainbow-test--has-face-p 13 'test-face-2))
    (should (verilog-rainbow-test--has-face-p 17 'test-face-1))))

(ert-deftest verilog-rainbow-test--no-op-when-mode-disabled ()
  "Function is a no-op when verilog-rainbow-mode is not enabled."
  (with-temp-buffer
    (insert "begin end")
    ;; verilog-rainbow-mode is nil by default in a fresh buffer
    (let ((rainbow-delimiters-pick-face-function #'verilog-rainbow-test--pick-face))
      (verilog-rainbow-mode--propertize-verilog nil)
      (should (null (get-text-property 1 'font-lock-face))))))

(ert-deftest verilog-rainbow-test--unmatched-end-clamped ()
  "An unmatched end at depth 0 receives no face and depth stays >= 0."
  ;; Buffer: "end begin end"
  ;;          1   5    11
  (verilog-rainbow-test--with-buffer "end begin end"
    (verilog-rainbow-mode--propertize-verilog nil)
    ;; stray "end" at depth 0: pick-face returns nil, so no face applied
    (should (null (get-text-property 1 'font-lock-face)))
    ;; subsequent begin/end still colored at depth 1
    (should (verilog-rainbow-test--has-face-p 5  'test-face-1))
    (should (verilog-rainbow-test--has-face-p 11 'test-face-1))))

(ert-deftest verilog-rainbow-test--skips-line-comments ()
  "begin/end inside // line comments are not colored."
  (skip-unless (fboundp 'verilog-mode))
  ;; Buffer: "// begin end\nbegin end"
  ;;                         14
  (with-temp-buffer
    (verilog-mode)
    (setq-local verilog-rainbow-mode t)
    (insert "// begin end\nbegin end")
    (let ((rainbow-delimiters-pick-face-function #'verilog-rainbow-test--pick-face))
      (verilog-rainbow-mode--propertize-verilog nil)
      ;; "begin" on line 1 is inside a comment — should not be colored
      (should (null (get-text-property 4 'font-lock-face)))
      ;; "begin" on line 2 should be colored at depth 1
      (should (verilog-rainbow-test--has-face-p 14 'test-face-1)))))

;;; Tests: test1.sv

(ert-deftest verilog-rainbow-test--test1-sv ()
  "test1.sv: verify begin/end depth sequence matches the nesting structure."
  (skip-unless (fboundp 'verilog-mode))
  (let ((path (expand-file-name
               "test/test1.sv"
               (file-name-directory (locate-library "verilog-rainbow-mode")))))
    (skip-unless (file-exists-p path))
    (with-temp-buffer
      (verilog-mode)
      (setq-local verilog-rainbow-mode t)
      (insert-file-contents path)
      (let ((rainbow-delimiters-pick-face-function #'verilog-rainbow-test--pick-face))
        (verilog-rainbow-mode--propertize-verilog nil)
        (let ((depths (verilog-rainbow-test--keyword-depths)))
          ;; Every begin depth should be >= 1
          (dolist (entry depths)
            (when (string= (car entry) "begin")
              (should (and (integerp (cadr entry))
                           (>= (cadr entry) 1)))))
          ;; Nesting should be balanced: all end depths >= 1
          (let ((ends (seq-filter (lambda (e) (string= (car e) "end")) depths)))
            (dolist (e ends)
              (should (and (integerp (cadr e)) (>= (cadr e) 1)))))
          ;; Depth increases with nesting: there must be at least one depth-2
          (should (seq-some (lambda (e) (and (string= (car e) "begin")
                                             (= (cadr e) 2)))
                            depths)))))))

;;; Tests: test2.sv

(ert-deftest verilog-rainbow-test--test2-sv-depths ()
  "test2.sv: triple-nested for-loops reach depth 4."
  (skip-unless (fboundp 'verilog-mode))
  (let ((path (expand-file-name
               "test/test2.sv"
               (file-name-directory (locate-library "verilog-rainbow-mode")))))
    (skip-unless (file-exists-p path))
    (with-temp-buffer
      (verilog-mode)
      (setq-local verilog-rainbow-mode t)
      (insert-file-contents path)
      (let ((rainbow-delimiters-pick-face-function #'verilog-rainbow-test--pick-face))
        (verilog-rainbow-mode--propertize-verilog nil)
        (let ((depths (mapcar #'cadr (verilog-rainbow-test--keyword-depths))))
          ;; always_comb begin + 3 for-loop begins + 3 for-loop ends + always_comb end = 8
          (should (= (length depths) 8))
          ;; always_comb begin → depth 1
          ;; outer/middle/inner for begins → 2 3 4
          ;; inner/middle/outer for ends  → 4 3 2
          ;; always_comb end → depth 1
          (should (equal depths '(1 2 3 4 4 3 2 1))))))))

;;; Tests: minor mode setup

(ert-deftest verilog-rainbow-test--mode-registers-jit-lock ()
  "Enabling verilog-rainbow-mode adds the jit-lock fontification function."
  (verilog-rainbow-test--with-buffer "begin end"
    (setq-local verilog-rainbow-mode nil)
    (verilog-rainbow-mode 1)
    (should (memq #'verilog-rainbow--jit-fontify jit-lock-functions))
    (verilog-rainbow-mode -1)))

(ert-deftest verilog-rainbow-test--mode-removes-jit-lock ()
  "Disabling verilog-rainbow-mode removes the jit-lock fontification function."
  (verilog-rainbow-test--with-buffer "begin end"
    (setq-local verilog-rainbow-mode nil)
    (verilog-rainbow-mode 1)
    (verilog-rainbow-mode -1)
    (should-not (memq #'verilog-rainbow--jit-fontify jit-lock-functions))))
