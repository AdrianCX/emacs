;;; yaml-mode.el --- minimal, dependency-free major mode for YAML  -*- lexical-binding: t; -*-

;; A small self-contained YAML mode.  No external packages, no tree-sitter,
;; so it stays portable: drop this file on `load-path' and `(require 'yaml-mode)'.
;; Provides basic syntax highlighting and space-based indentation.

;;; Code:

(defvar yaml-indent-offset 2
  "Number of spaces for each YAML indentation step.")

(defvar yaml-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Treat these as word/symbol constituents so keys highlight cleanly.
    ;; Comments and strings are handled by font-lock keywords (below) rather
    ;; than the syntax table, so that a `#' or a quote that lives *inside* the
    ;; other kind of quote is treated as ordinary text, not a new region.
    (modify-syntax-entry ?_  "w" table)
    (modify-syntax-entry ?-  "_" table)
    (modify-syntax-entry ?.  "_" table)
    ;; Demote both quote characters to plain punctuation.  The inherited
    ;; standard table makes ?\" a string delimiter, which would let the
    ;; syntactic pass fontify a " that lives inside a '...' scalar.  We do all
    ;; string highlighting via font-lock keywords instead, so neither quote
    ;; should carry string syntax here.
    (modify-syntax-entry ?\" "." table)
    (modify-syntax-entry ?\' "." table)
    table)
  "Syntax table for `yaml-mode'.")

(defvar yaml-font-lock-keywords
  `(;; Document / directive markers:  ---  ...  %YAML 1.2
    ("^\\(---\\|\\.\\.\\.\\|%.*\\)\\s-*$" . font-lock-comment-delimiter-face)
    ;; Quoted strings, matched as ONE left-to-right alternation so the first
    ;; opening quote owns the span: a " inside '...' (and a ' inside "...")
    ;; is then just literal text.  \\ escapes in "" and doubled '' in '' are
    ;; consumed too.  Both branches forbid newlines, so a closed string never
    ;; spills past the end of its line.
    ("\"\\(?:\\\\.\\|[^\"\\\n]\\)*\"\\|'\\(?:''\\|[^'\n]\\)*'"
     . font-lock-string-face)
    ;; Unterminated quotes: an opening quote with no partner on the line is
    ;; highlighted only to end of line.  The leading boundary keeps mid-word
    ;; apostrophes (don't, it's) from being treated as string openers.
    ("\\(?:^\\|[][ \t:,{}]\\)\\(\"[^\"\n]*\\)$" (1 font-lock-string-face))
    ("\\(?:^\\|[][ \t:,{}]\\)\\('[^'\n]*\\)$"   (1 font-lock-string-face))
    ;; Comments: `#' starts one only at line start or after whitespace (YAML
    ;; rule).  Placed after the string rules so a # inside a string stays a
    ;; string, and so it never runs past end of line.
    ("\\(?:^\\|[ \t]\\)\\(#.*\\)$" (1 font-lock-comment-face))
    ;; Mapping keys, optionally preceded by block-sequence dashes.
    ("^[ \t]*\\(?:-[ \t]+\\)*\\([^ \t\n:#&*!][^:#\n]*?\\)[ \t]*:\\(?:[ \t]\\|$\\)"
     (1 font-lock-variable-name-face))
    ;; Block sequence entry dash.
    ("^[ \t]*\\(-\\)\\(?:[ \t]\\|$\\)" (1 font-lock-keyword-face))
    ;; Anchors &name and aliases *name.
    ("[&*]\\([A-Za-z0-9_-]+\\)" (0 font-lock-type-face))
    ;; Tags such as !!str or !custom.
    ("\\(!!?[A-Za-z0-9_/-]*\\)" (1 font-lock-builtin-face))
    ;; Booleans and null in their common spellings.
    ("\\_<\\(true\\|false\\|yes\\|no\\|on\\|off\\|null\\|True\\|False\\|Null\\|TRUE\\|FALSE\\|NULL\\|~\\)\\_>"
     (1 font-lock-constant-face))
    ;; Numbers.
    ("\\_<-?[0-9]+\\(?:\\.[0-9]+\\)?\\(?:[eE][-+]?[0-9]+\\)?\\_>"
     . font-lock-constant-face))
  "Font-lock rules for `yaml-mode'.")

(defun yaml-indent-line ()
  "Indent the current YAML line using spaces only.
The first TAB lines the row up under the previous non-blank line; pressing
TAB again steps deeper by `yaml-indent-offset', then wraps back to column 0."
  (interactive)
  (let* ((indent-tabs-mode nil)
         (cur (current-indentation))
         (prev (save-excursion
                 (forward-line -1)
                 (while (and (not (bobp))
                             (looking-at-p "^[ \t]*\\(#.*\\)?$"))
                   (forward-line -1))
                 (current-indentation)))
         (target (cond
                  ;; Fresh request: line up under the previous row.
                  ((not (eq last-command 'yaml-indent-line)) prev)
                  ;; Repeated TAB: step in, wrapping back to 0 past the parent.
                  ((<= cur prev) (+ cur yaml-indent-offset))
                  (t 0))))
    (save-excursion (indent-line-to target))
    (when (< (current-column) target)
      (move-to-column target))))

;;;###autoload
(define-derived-mode yaml-mode prog-mode "YAML"
  "Simple major mode for editing YAML files."
  :syntax-table yaml-mode-syntax-table
  (setq-local font-lock-defaults '(yaml-font-lock-keywords))
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+ *")
  (setq-local comment-end "")
  (setq-local indent-tabs-mode nil)            ; YAML must use spaces.
  (setq-local tab-width yaml-indent-offset)
  (setq-local indent-line-function #'yaml-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

(provide 'yaml-mode)

;;; yaml-mode.el ends here
