;;; vscode-dark-theme.el --- dark theme approximating VS Code Dark+  -*- lexical-binding: t; -*-

;; A self-contained custom theme (no external packages) that mimics the
;; default VS Code "Dark+" palette, including the Visual Studio blue text
;; selection.  Enable with:  (load-theme 'vscode-dark t)

;;; Code:

(deftheme vscode-dark
  "Dark theme approximating Visual Studio Code's default Dark+ palette.")

(let ((bg            "#010101")   ; near-black editor background; NOT exact #000000 so
                                  ; terminal Emacs emits a 24-bit truecolor escape
                                  ; (48;2;1;1;1) instead of the legacy `ESC[40m' palette
                                  ; code, which renders as the terminal's (gray) ANSI color 0.
                                  ; Visually identical to pure black in a GUI.
      (fg            "#d4d4d4")   ; default foreground
      (cursor        "#aeafad")
      (selection     "#1e3a5c")   ; Visual Studio blue selection (darkened)
      (line-hl       "#1a1a1a")   ; current line / hover
      (comment       "#6a9955")   ; green
      (string        "#ce9178")   ; orange
      (keyword       "#3f7cb0")   ; blue (darkened)
      (type          "#4ec9b0")   ; teal
      (func          "#dcdcaa")   ; yellow
      (number        "#b5cea8")   ; light green
      (variable      "#80b4d0")   ; light blue (darkened)
      (control       "#c586c0")   ; purple (control keywords / builtins)
      (warning       "#d7ba7d")
      (error         "#f44747")
      (gutter        "#858585")
      (border        "#2d2d2d")
      (ml-bg         "#005a99")   ; active modeline = VS status bar blue (darkened)
      (ml-fg         "#ffffff")
      (ml-inact-bg   "#1a1a1a")
      (match-bg      "#3a3d41"))
  (custom-theme-set-faces
   'vscode-dark
   `(default            ((t (:background ,bg :foreground ,fg))))
   `(cursor             ((t (:background ,cursor))))
   `(region             ((t (:background ,selection :extend t))))
   `(highlight          ((t (:background ,line-hl))))
   `(hl-line            ((t (:background ,line-hl :extend t))))
   `(secondary-selection ((t (:background ,match-bg :extend t))))
   `(fringe             ((t (:background ,bg))))
   `(vertical-border    ((t (:foreground ,border))))
   `(minibuffer-prompt  ((t (:foreground ,keyword :weight bold))))
   `(link               ((t (:foreground ,keyword :underline t))))
   `(error              ((t (:foreground ,error :weight bold))))
   `(warning            ((t (:foreground ,warning :weight bold))))
   `(success            ((t (:foreground ,comment :weight bold))))

   ;; Syntax highlighting.
   `(font-lock-comment-face           ((t (:foreground ,comment :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comment))))
   `(font-lock-doc-face               ((t (:foreground ,comment :slant italic))))
   `(font-lock-string-face            ((t (:foreground ,string))))
   `(font-lock-keyword-face           ((t (:foreground ,keyword))))
   `(font-lock-builtin-face           ((t (:foreground ,control))))
   `(font-lock-preprocessor-face      ((t (:foreground ,control))))
   `(font-lock-type-face              ((t (:foreground ,type))))
   `(font-lock-function-name-face     ((t (:foreground ,func))))
   `(font-lock-variable-name-face     ((t (:foreground ,variable))))
   `(font-lock-constant-face          ((t (:foreground ,number))))
   `(font-lock-warning-face           ((t (:foreground ,warning :weight bold))))

   ;; Search.
   `(isearch        ((t (:background ,warning :foreground ,bg))))
   `(lazy-highlight ((t (:background "#515c6a" :foreground ,fg))))
   `(match          ((t (:background ,match-bg))))
   `(show-paren-match    ((t (:background "#3b514d" :weight bold))))
   `(show-paren-mismatch ((t (:background ,error :foreground ,fg))))

   ;; Mode line.
   `(mode-line          ((t (:background ,ml-bg :foreground ,ml-fg :box nil))))
   `(mode-line-inactive ((t (:background ,ml-inact-bg :foreground ,gutter :box nil))))

   ;; Line numbers.
   `(line-number              ((t (:foreground ,gutter :background ,bg))))
   `(line-number-current-line ((t (:foreground ,fg :background ,bg))))

   ;; ido completion.
   `(ido-first-match ((t (:foreground ,func :weight bold))))
   `(ido-only-match  ((t (:foreground ,comment))))
   `(ido-subdir      ((t (:foreground ,keyword))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'vscode-dark)

;;; vscode-dark-theme.el ends here
