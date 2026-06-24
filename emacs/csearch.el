;;; csearch.el --- mmap-based code search daemon interface -*- lexical-binding: t -*-

;; Drives cscope_mmap.py as a long-lived subprocess.  The daemon holds
;; an mmap of the concatenated source archive and answers SEARCH
;; queries over stdin/stdout.
;;
;; Usage from Emacs:
;;   M-x csearch-build       — (re)build the archive from cscope.files
;;   M-x csearch-pattern     — search for a regex
;;   M-x csearch-symbol      — search for symbol at point (word-bounded)
;;   M-x csearch-text        — literal (fixed-string) search
;;
;; In the *csearch* results buffer:
;;   RET  — jump to file:line
;;   SPC  — show file:line in other window without selecting
;;   n/p  — next/prev result line
;;   q    — bury buffer
;;   g    — rebuild archive
;;
;; The daemon is started lazily on first query and reused for the
;; session.  `csearch-stop' kills it.

;;; Code:

(defgroup csearch nil
  "Interface to the mmap code-search daemon."
  :group 'tools
  :prefix "csearch-")

(defcustom csearch-program
  (expand-file-name "/home/adrian/cscope_mmap.py"
                    (file-name-directory
                     (or load-file-name
                         (locate-library "csearch")
                         default-directory)))
  "Absolute path to cscope_mmap.py."
  :type 'string
  :group 'csearch)

(defcustom csearch-base "cscope_archive"
  "Base name for the .dat / .idx archive files."
  :type 'string
  :group 'csearch)


;;; Faces -------------------------------------------------------------------

(defface csearch-file
  '((t :inherit font-lock-keyword-face :bold t))
  "File names in results."
  :group 'csearch)

(defface csearch-line-number
  '((t :inherit font-lock-constant-face))
  "Line numbers in results."
  :group 'csearch)

(defface csearch-separator
  '((t :inherit font-lock-comment-face))
  "Separator lines."
  :group 'csearch)


;;; Internal state ----------------------------------------------------------

(defvar csearch--process nil "The daemon process.")
(defvar csearch--output  "" "Partial output accumulator.")
(defvar csearch--callback nil "Function called with completed response text.")
(defvar csearch--directory nil "Working directory of the daemon.")
(defvar csearch--marker nil "Marker for return-to-origin.")
(defvar csearch--marker-ring (make-ring 16) "Ring of origin markers.")


;;; Results buffer mode -----------------------------------------------------

(defvar csearch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'csearch-goto)
    (define-key map (kbd "SPC") #'csearch-show)
    (define-key map "n"   #'csearch-next)
    (define-key map "p"   #'csearch-prev)
    (define-key map "u"   #'csearch-pop-mark)
    (define-key map "q"   #'quit-window)
    (define-key map "g"   #'csearch-build)
    (define-key map "s"   #'csearch-pattern)
    (define-key map "S"   #'csearch-symbol)
    map)
  "Keymap for `csearch-mode'.")

(define-derived-mode csearch-mode special-mode "CSearch"
  "Major mode for csearch results.
\\{csearch-mode-map}"
  (setq truncate-lines t))


;;; Daemon management -------------------------------------------------------

(defun csearch--find-root ()
  "Walk up from `default-directory' to find cscope.files."
  (let ((dir default-directory))
    (while (and dir
                (not (file-exists-p (expand-file-name "cscope.files" dir))))
      (let ((parent (file-name-directory (directory-file-name dir))))
        (setq dir (if (string= parent dir) nil parent))))
    (or dir default-directory)))

(defun csearch--ensure ()
  "Start the daemon if it is not already running.  Returns the process."
  (unless (and csearch--process (process-live-p csearch--process))
    (let* ((dir (csearch--find-root))
           (default-directory dir))
      (setq csearch--directory dir
            csearch--output ""
            csearch--callback nil
            csearch--process
            (start-process "csearch" " *csearch-daemon*"
                           "python3" csearch-program
                           "serve" "-b" csearch-base))
      (set-process-filter csearch--process #'csearch--filter)
      (set-process-sentinel csearch--process #'csearch--sentinel)
      (set-process-query-on-exit-flag csearch--process nil)
      (message "csearch: daemon started in %s" dir)))
  csearch--process)

(defun csearch-stop ()
  "Stop the daemon."
  (interactive)
  (when (and csearch--process (process-live-p csearch--process))
    (process-send-string csearch--process "QUIT\n"))
  (setq csearch--process nil)
  (when (buffer-live-p csearch--fontify-buffer)
    (kill-buffer csearch--fontify-buffer)
    (setq csearch--fontify-buffer nil))
  (message "csearch: daemon stopped"))

(defun csearch--sentinel (_proc event)
  (let ((ev (string-trim event)))
    (unless (member ev '("finished" "deleted"))
      (message "csearch daemon: %s" ev))))

(defun csearch--filter (_proc output)
  "Accumulate OUTPUT; fire callback when --END-- arrives."
  (setq csearch--output (concat csearch--output output))
  (let ((end-pos (string-match "--END--\n" csearch--output)))
    (when end-pos
      (let ((text (substring csearch--output 0 end-pos))
            (cb csearch--callback))
        (setq csearch--output (substring csearch--output (match-end 0))
              csearch--callback nil)
        (when cb (funcall cb text))))))

(defun csearch--send (command callback)
  "Send COMMAND string to the daemon; call CALLBACK with response body."
  (csearch--ensure)
  (setq csearch--output ""
        csearch--callback callback)
  (process-send-string csearch--process (concat command "\n")))


;;; Displaying results ------------------------------------------------------

(defun csearch--short-path (file)
  "Return the last two path components (parent/filename) of FILE."
  (let ((name (file-name-nondirectory file))
        (dir  (file-name-directory file)))
    (if dir
        (concat (file-name-nondirectory (directory-file-name dir)) "/" name)
      name)))

(defvar csearch--fontify-buffer nil)

(defun csearch--fontify-c-line (str)
  "Return a copy of STR with C/C++ font-lock faces applied.
Each call is independent so highlighting never spans lines."
  (unless (buffer-live-p csearch--fontify-buffer)
    (setq csearch--fontify-buffer (generate-new-buffer " *csearch-fontify*"))
    (with-current-buffer csearch--fontify-buffer
      (c-mode)
      (font-lock-mode 1)))
  (with-current-buffer csearch--fontify-buffer
    (let ((inhibit-modification-hooks t))
      (erase-buffer)
      (insert str)
      (font-lock-ensure (point-min) (point-max))
      (buffer-substring (point-min) (point-max)))))

(defun csearch--display (text)
  "Parse TEXT (file:line:content lines) and populate *csearch* buffer.
Results are displayed in a flat grep-like format with aligned columns."
  (let ((buf (get-buffer-create "*csearch*"))
        (raw-lines (split-string text "\n" t))
        (entries nil)
        (errors nil)
        (max-path-len 0))
    ;; First pass: parse lines, compute max short-path length.
    (dolist (line raw-lines)
      (if (string-match "\\`ERROR" line)
          (push line errors)
        (when (string-match "\\`\\(.+?\\):\\([0-9]+\\):\\(.*\\)" line)
          (let* ((file  (match-string 1 line))
                 (lnum  (match-string 2 line))
                 (text  (match-string 3 line))
                 (short (csearch--short-path file))
                 (tag   (format "%s:%s:" short lnum))
                 (tlen  (length tag)))
            (when (> tlen max-path-len)
              (setq max-path-len tlen))
            (push (list file lnum text short tag tlen) entries)))))
    (setq entries (nreverse entries))
    ;; Second pass: render aligned output.
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (hits 0))
        (erase-buffer)
        (dolist (err errors)
          (insert (propertize (concat err "\n") 'face 'error)))
        (dolist (entry entries)
          (let* ((file  (nth 0 entry))
                 (lnum  (nth 1 entry))
                 (text  (nth 2 entry))
                 (short (nth 3 entry))
                 (tag   (nth 4 entry))
                 (tlen  (nth 5 entry))
                 (pad   (make-string (max 1 (- max-path-len tlen)) ?\s))
                 (start (point)))
            (insert (propertize short 'face 'csearch-file)
                    ":"
                    (propertize lnum 'face 'csearch-line-number)
                    ":"
                    pad (csearch--fontify-c-line text) "\n")
            (put-text-property start (point) 'csearch-file file)
            (put-text-property start (point) 'csearch-line
                               (string-to-number lnum))
            (cl-incf hits)))
        (goto-char (point-min))
        (when (zerop hits)
          (insert (propertize "No matches.\n" 'face 'csearch-separator)))
        (csearch-mode)))
    (pop-to-buffer buf)
    (message "csearch: %d hit%s" (length entries)
             (if (= (length entries) 1) "" "s"))))


;;; Navigation in results buffer --------------------------------------------

(defun csearch--nav-props (&optional pt)
  "Return (FILE . LINE) at PT or point, or nil."
  (let ((p (or pt (point))))
    (let ((f (get-text-property p 'csearch-file))
          (l (get-text-property p 'csearch-line)))
      (when (and f l) (cons f l)))))

(defun csearch-goto ()
  "Jump to the file and line at point."
  (interactive)
  (let ((nav (csearch--nav-props)))
    (unless nav (user-error "No result on this line"))
    (let ((file (car nav))
          (line (cdr nav)))
      (ring-insert csearch--marker-ring (point-marker))
      (find-file-other-window
       (if (file-name-absolute-p file) file
         (expand-file-name file csearch--directory)))
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun csearch-show ()
  "Show the result at point in other window without selecting it."
  (interactive)
  (let ((nav (csearch--nav-props)))
    (unless nav (user-error "No result on this line"))
    (let ((file (car nav))
          (line (cdr nav))
          (win (selected-window)))
      (save-selected-window
        (find-file-other-window
         (if (file-name-absolute-p file) file
           (expand-file-name file csearch--directory)))
        (goto-char (point-min))
        (forward-line (1- line)))
      (select-window win))))

(defun csearch-next ()
  "Move to the next result line."
  (interactive)
  (let ((target (next-single-property-change
                 (line-end-position) 'csearch-line)))
    (if target (goto-char target)
      (user-error "No more results"))))

(defun csearch-prev ()
  "Move to the previous result line."
  (interactive)
  (let ((target (previous-single-property-change
                 (line-beginning-position) 'csearch-line)))
    (when target
      (setq target (previous-single-property-change target 'csearch-line)))
    (if target (goto-char target)
      (user-error "No earlier results"))))

(defun csearch-pop-mark ()
  "Return to where the last `csearch-goto' was invoked."
  (interactive)
  (if (ring-empty-p csearch--marker-ring)
      (user-error "Mark ring is empty")
    (let ((marker (ring-remove csearch--marker-ring 0)))
      (switch-to-buffer (marker-buffer marker))
      (goto-char marker))))


;;; Interactive commands ----------------------------------------------------

(defun csearch--read (prompt)
  "Read a search string, defaulting to symbol at point."
  (let ((def (thing-at-point 'symbol t)))
    (read-string (if def (format "%s (default %s): " prompt def)
                   (concat prompt ": "))
                 nil nil def)))

(defun csearch-pattern (pattern)
  "Search for regex PATTERN in the source archive."
  (interactive (list (csearch--read "Search pattern")))
  (csearch--send (concat "SEARCH " pattern)
                 #'csearch--display))

(defun csearch-symbol (symbol)
  "Search for SYMBOL with word boundaries."
  (interactive (list (csearch--read "Find symbol")))
  (csearch--send (format "SEARCH \\b%s\\b" (replace-regexp-in-string
                                              "[\\\\.*+?^${}()|\\[\\]]"
                                              "\\\\\\&" symbol))
                 #'csearch--display))

(defun csearch-text (text)
  "Literal (fixed-string) search for TEXT."
  (interactive (list (csearch--read "Find text")))
  (csearch--send (concat "SEARCH -F " text)
                 #'csearch--display))

(defun csearch-build ()
  "Rebuild the source archive from cscope.files."
  (interactive)
  (csearch--send "REBUILD"
                 (lambda (_) (message "csearch: rebuild complete"))))

;;; Global keybindings ------------------------------------------------------

(provide 'csearch)
;;; csearch.el ends here
