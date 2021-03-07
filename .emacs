(package-initialize)
(add-to-list 'load-path "~/emacs")

; general settings
(cua-mode t)
(menu-bar-mode -1)
(setq column-number-mode t)
(setq cua-auto-tabify-rectangles nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)
;(toggle-scroll-bar -1)
(tool-bar-mode -1)
(transient-mark-mode 1)
(delete-selection-mode 1)
(setq require-final-newline nil)
(setq mode-require-final-newline nil)
(set-default-coding-systems 'utf-8)

;; remember cursor position
(if (version< emacs-version "25.0") (progn (require 'saveplace) (setq-default save-place t)) (save-place-mode 1))

; delete trailing whitespaces
(add-hook 'before-save-hook '(lambda() (when (not (or (derived-mode-p 'markdown-mode))) (delete-trailing-whitespace))))

; find file at position
(ffap-bindings)

; prevent upcase shortcut accidents
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

; stop creating auto-save files
(setq auto-save-default nil)
(setq make-backup-files nil)

; Avoid annyoing splits
(setq split-height-threshold 500)
(setq split-width-threshold 500)

; c code defaults
(setq c-default-style "bsd" c-basic-offset 4)
(c-set-offset 'case-label '+)
(setq-default c-basic-offset 4 tab-width 4)
(setq-default c-basic-offset 4)

; shortcuts
(define-key global-map [(f9)]  'compile)

; Get /path/to/filename in clipboard
(defun put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (concat (buffer-file-name (window-buffer (minibuffer-selected-window))) ":" (number-to-string (line-number-at-pos))))))
    (when filename
      (kill-new filename)
      (message "%s" filename))))


(global-set-key (kbd "C-x f") 'put-file-name-on-clipboard)

; Get /path/to/filename in clipboard
(defun put-debug-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
(interactive)
  (insert (concat "println!(\"" (buffer-file-name (window-buffer (minibuffer-selected-window))) ":" (number-to-string (line-number-at-pos)) "\");"))
  (indent-region))

(global-set-key (kbd "C-x ,") 'put-debug-file-name-on-clipboard)


; Highlight fun
(global-set-key (kbd "C-x ;") 'highlight-symbol-at-point)
(global-set-key (kbd "C-x '") 'unhighlight-regexp)

; Fix control+... in weird use cases
(define-key input-decode-map "\e[1;5A" [C-up])
(define-key input-decode-map "\e[1;5B" [C-down])
(define-key input-decode-map "\e[1;5C" [C-right])
(define-key input-decode-map "\e[1;5D" [C-left])
(global-set-key [(control ?h)] 'backward-kill-word)

; Use C-x in term instead of C-c
(add-hook 'term-mode-hook
  (lambda ()
    ;; C-x is the prefix command, rather than C-c
    (term-set-escape-char ?\C-x)
      (define-key term-raw-map "\M-y" 'yank-pop)
      (define-key term-raw-map "\M-w" 'kill-ring-save)))

; Command for multiple terminals
(defvar counter 0)
(defun sh ()
  "Open a new terminal and rename the buffer"
  (interactive)
  (setq counter (+ counter 1))
  (setq title (concat "Terminal-" (number-to-string counter)))
  (setq buf-title (concat "*" title "*"))
  (message buf-title)
  (set-buffer (make-term title "/bin/bash"))
  (term-mode)
  (term-char-mode)
  (switch-to-buffer buf-title))


; Have key bindings same as GDB tui mode - enabled via C-x t
(defvar tui-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") 'gud-step)
    (define-key map (kbd "f") 'gud-finish)
    (define-key map (kbd "c") 'gud-cont)
    (define-key map (kbd "p") 'gud-print)
    (define-key map (kbd "u") 'gud-up)
    (define-key map (kbd "d") 'gud-down)
    (define-key map (kbd "r") 'gud-run)
    (define-key map (kbd "n") 'gud-next)
    map)
  "tui-minor-mode keymap.")

(define-minor-mode tui-minor-mode
  "GDB minor mode to provide TUI keys."
  :init-value nil
  :lighter " tui")

(define-global-minor-mode tui-mode tui-minor-mode
  (lambda () (tui-minor-mode 1)))

(add-hook 'gdb-mode-hook (lambda () (tui-minor-mode 0)))
(global-set-key (kbd "C-x t") 'tui-minor-mode)

; Configure GDB layout split vertically - code 80%, output 10%, gdb 10%
(setq gdb-many-windows nil)

(defun set-gdb-layout(&optional c-buffer)
  (if (not c-buffer) (setq c-buffer (window-buffer (selected-window)))) ;; save current buffer

  (set-window-dedicated-p (selected-window) nil) ;; unset dedicate state if needed
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows) ;; clean all

  (let* ((w-source (selected-window))
         (w-gdb (split-window w-source (floor(* 0.9 (window-body-height))) 'below))
         (w-io (split-window w-source (floor(* 0.9 (window-body-height))) 'below)))
    (set-window-buffer w-io (gdb-get-buffer-create 'gdb-inferior-io))
    (set-window-dedicated-p w-io t)
    (set-window-buffer w-gdb gud-comint-buffer)

    (select-window w-gdb)
    (set-window-buffer w-source c-buffer)))

(defadvice gdb (around args activate)
  "Change the way to gdb works."
  (setq global-config-editing (current-window-configuration)) ;; to restore: (set-window-configuration c-editing)
  (let ((c-buffer (window-buffer (selected-window))))
    ad-do-it
    (set-gdb-layout c-buffer)))

(defadvice gdb-reset (around args activate)
  "Change the way to gdb exit."
  ad-do-it
  (set-window-configuration global-config-editing))

; Edit files via ssh
(require 'tramp)
(setq tramp-default-method "ssh")

; Fix X11 copy paste
;(require 'xclip)
;(xclip-mode 1)

; code browsing
(require 'xcscope)
(define-key global-map [(control f5)]  'cscope-find-this-symbol)
(setq cscope-option-use-inverted-index t)

;; quick helpers to load project files
(defvar src-path nil "path source code")
(defvar src-files nil "list of cscope files")
(defvar cscope-file-list nil "/path/to/cscope.files")

(defun slurp (f) (with-temp-buffer (insert-file-contents f) (buffer-substring-no-properties (point-min) (point-max))))

(defun load-project  (source-path)
  "load a cscope.files file."
  (interactive "fpath to src: ")
  (setq ido-virtual-buffers '())
  ;(setq recentf-list '())
  (setq src-path source-path)
  (setq src-files (concat src-path "cscope.files"))
  (setq cscope-file-list (split-string (slurp src-files)))
  (cscope-set-initial-directory src-path)
  (grep-compute-defaults)
  (grep-apply-setting 'grep-command (concat (concat "grep -nr "))))

; Code browsing via ido - C-w - load list of project files
(require 'ido)
(setq ido-enable-flex-matching t)
(ido-mode 'buffers) ;; only use this line to turn off ido for file names!
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*" "*Messages*" "Async Shell Command"))

(defun clear-recentf ()
  "Clear recentf"
  (interactive)
  (setq recentf-list '()))


(recentf-mode 1)
(setq recentf-max-menu-items 1000)
(setq recentf-max-saved-items 1000)

(defun ido-choose-from-recentf ()
  "Use ido to select a recently visited file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))

(defun ido-choose-from-cscope ()
  "Use ido to select a file from a cscope.files list"
  (interactive)
  (if cscope-file-list nil (setq  cscope-file-list (split-string (replace-regexp-in-string "^" src-path (slurp src-files) "\n" t))))
  (find-file (ido-completing-read "Open file: " (append recentf-list cscope-file-list) nil t)))

(global-set-key (kbd "C-e") 'ido-choose-from-recentf)
(global-set-key (kbd "C-d") 'load-project)
(global-set-key (kbd "C-w") 'ido-choose-from-cscope)

(global-set-key (kbd "M-e") 'ido-choose-from-recentf)
(global-set-key (kbd "M-d") 'load-project)
(global-set-key (kbd "M-w") 'ido-choose-from-cscope)

; Useful when going through lots of logs - hide lines matching regex, not matching regex, etc.
(require 'hide-lines)

(defvar hide-mode nil "hide matching or not")
(defvar hide-lines-column nil "column for hide-lines")
(defvar hide-lines-text nil "text for hide-lines")

(defun get-selected-text ()
  (when (region-active-p)
    (let (deactivate-mark)
      (buffer-substring (region-beginning) (region-end)))))

(defun hide-not-matching ()
  (interactive)
  (setq hide-mode nil)
  (setq hide-lines-column (current-column))
  (setq hide-lines-text (get-selected-text))
  (hide-lines-not-matching hide-lines-text)
  (move-to-column hide-lines-column t)
  (keyboard-quit))

(defun hide-matching ()
  (interactive)
  (setq hide-mode t)
  (setq hide-lines-column (save-excursion (goto-char (region-beginning)) (current-column)))
  (setq hide-lines-text (get-selected-text))
  (hide-lines-matching hide-lines-text)
  (move-to-column hide-lines-column t)
  (keyboard-quit))

(defun rehide-lines ()
  (interactive)
  (if hide-mode
      (hide-lines-matching hide-lines-text)
    (hide-lines-not-matching hide-lines-text))
  (move-to-column hide-lines-column t)
  (keyboard-quit))

(global-set-key (kbd "C-,") 'hide-not-matching)
(global-set-key (kbd "C-.") 'hide-matching)
(global-set-key (kbd "M-<down>") 'hide-lines-show-all)
(global-set-key (kbd "M-<up>") 'rehide-lines)
(global-set-key (kbd "M-<up>") 'hide-lines-not-matching-ni)
(global-set-key (kbd "M-<down>") 'hide-lines-show-all)


; Customization per project

; rust flags: RUSTFLAGS="-Z macro-backtrace -Z print-link-args"
; docker flags: --cap-add=SYS_PTRACE
; cargo: -- --nocapture

; Customize as needed

;(defun app-gdb ()
;  "Simplify workflow"
;  (interactive)
;  (gdb "rust-gdb -i=mi --args /path/to/rust-unittest --nocapture testname"))


;(defun app-compile()
;  "Simplify workflow"
;  (interactive)
;  (setq compile-command "cd /path/to/proj/ && compile"))
