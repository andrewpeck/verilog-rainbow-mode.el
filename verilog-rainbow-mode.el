(defun rainbow-delimiters--apply-color-range (start end depth match)
  "Highlight a single delimiter at LOC according to DEPTH.

LOC is the location of the character to add text properties to.
DEPTH is the nested depth at LOC, which determines the face to use.
MATCH is nil iff it's a mismatched closing delimiter."
  (let ((face (funcall rainbow-delimiters-pick-face-function depth match start)))
    (when face
      (font-lock-prepend-text-property start end 'font-lock-face face))))

(defun rainbow-delimiters--propertize-verilog (_)
  "Highlight delimiters in region between point and END.

Used by font-lock for dynamic highlighting."

  (interactive)
  (when (eq major-mode 'verilog-mode)
    (goto-char (point-min))
    (let* ((depth 0)
           (_ (point-max)))

      ;; find begin-end pairs in the whole doc
      ;; should probably operate on a range in case the document is large :(
      ;; but for now this works...
      (while (re-search-forward "\\<begin\\>\\|\\<end\\>" nil t)

        ;; skip if we are in a comment
        (unless (nth 4 (syntax-ppss))

          (let* ((delim-start (match-beginning 0))
                 (delim-end (match-end 0))
                 (delim-word (match-string-no-properties 0)))

            (when (string= delim-word "begin")
              (setq depth (1+ depth)))

            (rainbow-delimiters--apply-color-range delim-start delim-end depth (> depth 0))

            (when (string= delim-word "end")
              (setq depth (1- depth)))

            (when (= depth -1)
              (setq depth 0)))))))
  ;; We already fontified the delimiters, tell font-lock there's nothing more
  ;; to do.
  nil)
