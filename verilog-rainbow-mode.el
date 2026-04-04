;;; verilog-rainbow-mode.el --- Functions for working with verilog files -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023-2026 Andrew Peck

;; Author: Andrew Peck <peckandrew@gmail.com>
;; URL: https://github.com/andrewpeck/verilog-rainbow-mode.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (rainbow-delimiters "2.0"))
;; Keywords: tools verilog

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>

;;; Commentary:
;;
;; Provides nested coloring for verilog begin/end keywords using the
;; same face palette as rainbow-delimiters.  Works with both
;; `verilog-mode' and treesitter-based modes such as `verilog-ts-mode'.
;;
;;; Code:

;; Declare as special so (let ((rainbow-delimiters-pick-face-function ...)))
;; creates a dynamic binding.  rainbow-delimiters sets the actual value via
;; defcustom when loaded; we just need the declaration here.
(defvar rainbow-delimiters-pick-face-function)

;; Forward declaration: defined by define-minor-mode below.
(defvar verilog-rainbow-mode)

(defun verilog-rainbow-mode--apply-color-range (start end depth match)
  "Apply a rainbow face to the delimiter spanning START to END.

DEPTH is the nesting depth, which selects the face.
MATCH is nil if this is a mismatched closing delimiter."
  (let ((face (funcall rainbow-delimiters-pick-face-function depth match start)))
    (when face
      ;; Use put-text-property (replace) rather than font-lock-prepend-text-property
      ;; (prepend+dedup).  Prepend's dedup check prevents the face from updating
      ;; when depth decreases: the lower-depth face is already in the accumulated
      ;; list so memq finds it and does nothing, leaving the stale higher-depth
      ;; face at the front.  Replacing is correct because we always rescan the
      ;; whole buffer with fresh depth counts.
      (put-text-property start end 'font-lock-face face))))

(defun verilog-rainbow-mode--propertize-verilog (_limit)
  "Color all begin/end keywords in the buffer by nesting depth.

Used as a jit-lock fontification function.  Ignores _LIMIT and
always scans the whole buffer because nesting depth is global state."
  (when verilog-rainbow-mode
    (goto-char (point-min))
    (let ((depth 0))
      (while (re-search-forward "\\<begin\\>\\|\\<end\\>" nil t)
        (unless (nth 4 (syntax-ppss))
          (let ((delim-start (match-beginning 0))
                (delim-end   (match-end 0))
                (delim-word  (match-string-no-properties 0)))
            (when (string= delim-word "begin")
              (setq depth (1+ depth)))
            (verilog-rainbow-mode--apply-color-range delim-start delim-end depth (> depth 0))
            (when (string= delim-word "end")
              (setq depth (1- depth)))
            (when (= depth -1)
              (setq depth 0)))))))
  nil)

(defun verilog-rainbow--jit-fontify (_start _end)
  "JIT-lock entry point for verilog rainbow fontification.

_START and _END are the requested region but are ignored; nesting
depth is global state so the whole buffer is always scanned."
  (save-excursion
    (verilog-rainbow-mode--propertize-verilog nil)))

;;;###autoload
(define-minor-mode verilog-rainbow-mode
  "Color Verilog begin/end keywords by nesting depth like rainbow-delimiters.

Uses `jit-lock-functions' at depth 90 so it runs after both
traditional font-lock and treesitter-based fontification."
  :lighter ""
  (if verilog-rainbow-mode
      ;; Depth 90 ensures we run after font-lock/treesitter (depth 0).
      (add-hook 'jit-lock-functions #'verilog-rainbow--jit-fontify 90 t)
    (remove-hook 'jit-lock-functions #'verilog-rainbow--jit-fontify t))
  ;; Defer refontification: when called from a mode hook, font-lock and
  ;; jit-lock are not yet fully initialized (that happens in
  ;; `change-major-mode-after-body-hook', which runs after mode hooks).
  ;; A zero-second idle timer fires after all setup is complete.
  (let ((buf (current-buffer)))
    (run-with-idle-timer
     0 nil (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (jit-lock-refontify)))))))

(provide 'verilog-rainbow-mode)
;;; verilog-rainbow-mode.el ends here
;; LocalWords:  matcher fontification verilog treesitter
