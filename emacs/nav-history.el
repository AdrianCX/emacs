;;; nav-history.el --- Navigation history with back/forward -*- lexical-binding: t -*-

;;; Code:

(defvar nav-history--list nil "List of (NAME . LINE) entries, newest first. NAME is a file path or buffer name.")
(defvar nav-history--index -1 "Current position in `nav-history--list'. -1 means at head.")
(defvar nav-history--max 100 "Maximum number of entries to keep.")
(defvar nav-history--inhibit nil "When non-nil, don't record.")
(defvar nav-history--last-buffer nil "Previously seen buffer.")
(defvar nav-history--last-name nil "File path or buffer name of previously seen buffer.")
(defvar nav-history--last-line nil "Line number in previously seen buffer.")

(defun nav-history--current-name ()
  "Return file path for file buffers, buffer name for visible non-file buffers, or nil to skip."
  (cond
   ((minibufferp) nil)
   ((buffer-file-name))
   ((string-prefix-p " " (buffer-name)) nil)
   ((string= (buffer-name) "*nav-history*") nil)
   (t (buffer-name))))

(defun nav-history--push (name line)
  "Push NAME:LINE onto history, truncating forward history."
  (let ((entry (cons name line)))
    (when (> nav-history--index 0)
      (setq nav-history--list (nthcdr nav-history--index nav-history--list))
      (setq nav-history--index 0))
    (unless (equal entry (car nav-history--list))
      (push entry nav-history--list)
      (when (> (length nav-history--list) nav-history--max)
        (setcdr (nthcdr (1- nav-history--max) nav-history--list) nil))
      (setq nav-history--index 0))))

(defun nav-history--track ()
  "Detect buffer switches and record the previous location."
  (when (not nav-history--inhibit)
    (let ((cur-buf (current-buffer))
          (cur-name (nav-history--current-name)))
      (when (and nav-history--last-name
                 (not (eq cur-buf nav-history--last-buffer)))
        (nav-history--push nav-history--last-name nav-history--last-line))
      (when cur-name
        (setq nav-history--last-buffer cur-buf
              nav-history--last-name cur-name
              nav-history--last-line (line-number-at-pos))))))

(defun nav-history--is-file-p (name)
  "Return non-nil if NAME is a file path rather than a buffer name."
  (string-prefix-p "/" name))

(defun nav-history--goto (entry)
  "Jump to ENTRY (NAME . LINE) without recording.
If the target buffer is already visible in a window, select that
window instead of replacing the current buffer."
  (let ((nav-history--inhibit t)
        (name (car entry))
        (line (cdr entry))
        (ok t))
    (let ((buf (if (nav-history--is-file-p name)
                   (or (find-buffer-visiting name)
                       (find-file-noselect name))
                 (get-buffer name))))
      (if (not buf)
          (progn (message "Buffer %s no longer exists" name)
                 (setq ok nil))
        (let ((win (get-buffer-window buf)))
          (if win
              (select-window win)
            (switch-to-buffer buf)))))
    (when ok
      (goto-char (point-min))
      (forward-line (1- line))
      (setq nav-history--last-buffer (current-buffer)
            nav-history--last-name (nav-history--current-name)
            nav-history--last-line line))))

(defun nav-history--save-current ()
  "Push current location if it differs from head of history."
  (let ((name (nav-history--current-name)))
    (when name
      (let ((cur (cons name (line-number-at-pos))))
        (when (and (= nav-history--index 0)
                   (not (equal cur (car nav-history--list))))
          (push cur nav-history--list)
          (setq nav-history--index 0))))))

(defun nav-history-back ()
  "Go back in navigation history."
  (interactive)
  (nav-history--save-current)
  (let ((next (1+ nav-history--index)))
    (if (>= next (length nav-history--list))
        (message "No older history")
      (setq nav-history--index next)
      (nav-history--goto (nth nav-history--index nav-history--list)))))

(defun nav-history-forward ()
  "Go forward in navigation history."
  (interactive)
  (if (<= nav-history--index 0)
      (message "No newer history")
    (setq nav-history--index (1- nav-history--index))
    (nav-history--goto (nth nav-history--index nav-history--list))))

(defun nav-history--display-name (name)
  "Return a short display string for NAME."
  (if (nav-history--is-file-p name)
      (let ((short (file-name-nondirectory name))
            (dir (file-name-nondirectory
                  (directory-file-name
                   (file-name-directory name)))))
        (format "%s/%s" dir short))
    name))

(defun nav-history-show ()
  "Display the navigation history in a buffer."
  (interactive)
  (let ((buf (get-buffer-create "*nav-history*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Navigation history (%d entries)\n\n"
                        (length nav-history--list)))
        (let ((i 0))
          (dolist (entry nav-history--list)
            (let ((marker (if (= i nav-history--index) ">> " "   ")))
              (insert (propertize
                       (format "%s%s:%d\n" marker
                               (nav-history--display-name (car entry))
                               (cdr entry))
                       'nav-history-entry entry
                       'nav-history-idx i)))
            (setq i (1+ i))))
        (goto-char (point-min))
        (nav-history-list-mode)))
    (display-buffer buf)))

(defvar nav-history-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'nav-history-list-goto)
    (define-key map "q" #'quit-window)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    map))

(define-derived-mode nav-history-list-mode special-mode "NavHist"
  "Mode for viewing navigation history."
  (setq truncate-lines t))

(defun nav-history-list-goto ()
  "Jump to the history entry on this line."
  (interactive)
  (let ((entry (get-text-property (point) 'nav-history-entry))
        (idx   (get-text-property (point) 'nav-history-idx)))
    (unless entry (user-error "No entry on this line"))
    (setq nav-history--index idx)
    (quit-window)
    (nav-history--goto entry)))

;;;###autoload
(define-minor-mode nav-history-mode
  "Global minor mode for navigation history."
  :global t
  :lighter " NH"
  (if nav-history-mode
      (progn
        (global-set-key (kbd "M-<left>")  #'nav-history-back)
        (global-set-key (kbd "M-<right>") #'nav-history-forward)
        (global-set-key (kbd "M-<up>")    #'nav-history-show)
        (add-hook 'post-command-hook #'nav-history--track))
    (global-unset-key (kbd "M-<left>"))
    (global-unset-key (kbd "M-<right>"))
    (global-unset-key (kbd "M-<up>"))
    (remove-hook 'post-command-hook #'nav-history--track)))

(provide 'nav-history)
;;; nav-history.el ends here
