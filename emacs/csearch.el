;;; csearch.el --- mmap-based code search daemon interface -*- lexical-binding: t -*-

(require 'subr-x)
(require 'cl-lib)
(require 'seq)
(require 'ring)

;; Drives cscope_mmap.py as a long-lived subprocess.  The daemon holds
;; an mmap of the concatenated source archive and answers SEARCH
;; queries over stdin/stdout.
;;
;; Usage from Emacs:
;;   M-x csearch-build       — (re)build the archive from cscope.files
;;   M-x csearch-pattern     — search for a regex
;;   M-x csearch-symbol      — search for symbol at point (word-bounded)
;;   M-x csearch-text        — literal (fixed-string) search
;;   M-x csearch-set-root    — point csearch at a project (sets `csearch-root')
;;   M-x csearch-diagnose    — show the paths in use and the daemon's view
;;   M-x csearch-restart     — throw the daemon away and start a new one
;;
;; Anything that goes wrong — a missing cscope.files, an unreadable
;; archive, a daemon that dies or stops answering — lands in the
;; *csearch-error* buffer together with the paths csearch is using and
;; the daemon's stderr.
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

(defcustom csearch-files-name "cscope.files"
  "Name of the file list a rebuild reads, looked up in the project root."
  :type 'string
  :group 'csearch)

(defcustom csearch-timeout 300
  "Seconds to wait for a daemon response before declaring it stuck."
  :type 'number
  :group 'csearch)

(defvar csearch-root nil
  "Project root: the directory holding `csearch-files-name'.
When nil the root is guessed by walking up from `default-directory',
which only works when the current buffer lives inside the project -- press
`\\[csearch-build]' from a scratch buffer or a dired of your home and the
guess lands somewhere with no file list, which is why a rebuild can fail
while searching against an already-built archive keeps working.

Set it explicitly with `csearch-set-root', or from a project loader:

    (setq csearch-root \"/path/to/project/\")")


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
(defvar csearch--pending nil "Command currently awaiting a response, or nil.")
(defvar csearch--timer nil "Timer guarding the pending command.")
(defvar csearch--fontify-buffer nil "Scratch buffer used for C font-locking.")
(defvar csearch--marker nil "Marker for return-to-origin.")
(defvar csearch--marker-ring (make-ring 16) "Ring of origin markers.")
(defvar csearch--origin nil
  "Absolute path of the file a search was triggered from.
Results are sorted by path closeness to this file.")


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
    (define-key map "?"   #'csearch-diagnose)
    map)
  "Keymap for `csearch-mode'.")

(define-derived-mode csearch-mode special-mode "CSearch"
  "Major mode for csearch results.
\\{csearch-mode-map}"
  (setq truncate-lines t))


;;; Daemon management -------------------------------------------------------

(defun csearch--find-root ()
  "Walk up from `default-directory' looking for `csearch-files-name'.
Returns nil when no ancestor directory holds one -- unlike a silent
fallback to `default-directory', that lets the caller say so."
  (let ((dir (expand-file-name default-directory)))
    (while (and dir
                (not (file-exists-p (expand-file-name csearch-files-name dir))))
      (let ((parent (file-name-directory (directory-file-name dir))))
        (setq dir (if (string= parent dir) nil parent))))
    dir))

(defun csearch--root (&optional strict)
  "Return the project root: `csearch-root', else the walk-up guess.
With STRICT, signal a `user-error' naming the problem when no root with a
`csearch-files-name' in it can be found."
  (let ((root (or csearch-root (csearch--find-root))))
    (cond
     (root (file-name-as-directory (expand-file-name root)))
     (strict
      (csearch--report
       (format "csearch: no %s found" csearch-files-name)
       (format "Walked up from %s to / without finding %s, and `csearch-root'\n\
is not set.  Fix it with:\n\n    M-x csearch-set-root RET /path/to/project/ RET\n\n\
or set it once from your project loader:\n\n    (setq csearch-root source-path)"
               default-directory csearch-files-name)
       t)
      (user-error "csearch: no %s above %s; use M-x csearch-set-root"
                  csearch-files-name default-directory))
     (t (file-name-as-directory (expand-file-name default-directory))))))

(defun csearch-set-root (dir)
  "Set `csearch-root' to DIR and restart the daemon there."
  (interactive "DProject root (holding cscope.files): ")
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (unless (file-exists-p (expand-file-name csearch-files-name dir))
      (unless (y-or-n-p (format "No %s in %s -- use it anyway? "
                                csearch-files-name dir))
        (user-error "csearch: root unchanged")))
    (setq csearch-root dir)
    (csearch--kill)
    (message "csearch: root set to %s" dir)))


;;; Error reporting ---------------------------------------------------------

(defconst csearch--stderr-name " *csearch-stderr*"
  "Buffer holding the daemon's stderr, kept out of the response stream.")

(defun csearch--stderr-text ()
  "Return the daemon's stderr output, or nil when there is none."
  (let ((buf (get-buffer csearch--stderr-name)))
    (when (buffer-live-p buf)
      (let ((s (string-trim (with-current-buffer buf (buffer-string)))))
        (unless (string-empty-p s) s)))))

(defun csearch--context ()
  "Describe the current configuration, flagging whatever is missing."
  (let* ((root (or csearch-root (csearch--find-root)))
         (root (and root (file-name-as-directory (expand-file-name root))))
         (flist (and root (expand-file-name csearch-files-name root)))
         (dat (and root (concat (expand-file-name csearch-base root) ".dat"))))
    (concat
     (format "program    %s%s\n" csearch-program
             (if (file-readable-p csearch-program) "" "   [NOT READABLE]"))
     (format "root       %s\n"
             (cond ((null root) (format "UNKNOWN -- no %s above %s"
                                        csearch-files-name default-directory))
                   ((file-directory-p root) root)
                   (t (concat root "   [NOT A DIRECTORY]"))))
     (format "           %s\n"
             (if csearch-root "from `csearch-root'"
               "guessed from the current buffer -- set `csearch-root' to pin it"))
     (format "file list  %s%s\n" (or flist "-")
             (if (and flist (file-readable-p flist)) "" "   [MISSING]"))
     (format "archive    %s%s\n" (or dat "-")
             (if (and dat (file-exists-p dat))
                 (format "   (%d bytes)" (file-attribute-size
                                          (file-attributes dat)))
               "   [MISSING]"))
     (format "daemon     %s\n"
             (if (process-live-p csearch--process)
                 (format "running (pid %s) in %s"
                         (process-id csearch--process) csearch--directory)
               "not running")))))

(defun csearch--report (title detail &optional errorp quiet)
  "Show TITLE, the current context and DETAIL in the *csearch-error* buffer.
With QUIET, fill the buffer but leave it unshown -- the echo area still
points at it, so a rebuild that merely skipped a few files does not steal
a window."
  (let ((buf (get-buffer-create "*csearch-error*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize title 'face (if errorp 'error 'bold)) "\n\n")
        (insert (csearch--context) "\n")
        (when (and detail (not (string-empty-p (string-trim detail))))
          (insert "--- daemon output ---\n" (string-trim detail) "\n\n"))
        (let ((err (csearch--stderr-text)))
          (when err
            (insert "--- daemon stderr ---\n" err "\n")))
        (goto-char (point-min))
        (special-mode)))
    (unless quiet (display-buffer buf))
    (message "%s -- see *csearch-error*" title)))

(defun csearch--errors (text)
  "Return the ERROR lines of TEXT as a list."
  (seq-filter (lambda (l) (string-prefix-p "ERROR" l))
              (split-string (or text "") "\n" t)))


;;; Daemon lifecycle --------------------------------------------------------

(defun csearch--cancel-timer ()
  (when csearch--timer
    (cancel-timer csearch--timer)
    (setq csearch--timer nil)))

(defun csearch--kill (&optional quietly)
  "Terminate the daemon, dropping any pending request."
  (csearch--cancel-timer)
  (setq csearch--callback nil
        csearch--pending nil)
  (when (process-live-p csearch--process)
    (set-process-sentinel csearch--process #'ignore)
    (ignore-errors (process-send-string csearch--process "QUIT\n"))
    (ignore-errors (delete-process csearch--process))
    (unless quietly (message "csearch: daemon stopped")))
  (setq csearch--process nil))

(defun csearch--ensure ()
  "Start the daemon if needed, in the current root.  Returns the process."
  (let ((root (csearch--root t)))
    ;; The daemon's cwd is fixed at startup, so a root change means a
    ;; restart -- otherwise it would keep answering about the old project.
    (when (and (process-live-p csearch--process)
               (not (equal csearch--directory root)))
      (csearch--kill t))
    (unless (process-live-p csearch--process)
      (unless (file-readable-p csearch-program)
        (csearch--report "csearch: daemon program not found"
                         (format "`csearch-program' points at\n  %s\nwhich does \
not exist or is not readable.\nSet it with M-x customize-variable RET \
csearch-program RET." csearch-program)
                         t)
        (user-error "csearch: no such program: %s" csearch-program))
      (let ((default-directory root)
            (stderr (get-buffer-create csearch--stderr-name)))
        (with-current-buffer stderr (erase-buffer))
        (setq csearch--directory root
              csearch--output ""
              csearch--callback nil
              csearch--pending nil
              csearch--process
              (make-process :name "csearch"
                            :buffer nil
                            :command (list "python3" csearch-program
                                           "serve" "-b" csearch-base)
                            :connection-type 'pipe
                            :noquery t
                            :stderr stderr
                            :filter #'csearch--filter
                            :sentinel #'csearch--sentinel))
        ;; Emacs gives the :stderr buffer its own pipe process, whose
        ;; sentinel would otherwise write "finished" into the very buffer
        ;; we quote back to the user.
        (let ((pipe (get-buffer-process stderr)))
          (when pipe (set-process-sentinel pipe #'ignore)))
        (message "csearch: daemon started in %s" root))))
  csearch--process)

(defun csearch-stop ()
  "Stop the daemon."
  (interactive)
  (csearch--kill)
  (when (buffer-live-p csearch--fontify-buffer)
    (kill-buffer csearch--fontify-buffer)
    (setq csearch--fontify-buffer nil)))

(defun csearch-restart ()
  "Stop the daemon and start a fresh one in the current root."
  (interactive)
  (csearch--kill t)
  (csearch--ensure))

(defun csearch--sentinel (proc event)
  "Report an unexpected daemon exit instead of leaving the caller hanging."
  (when (eq proc csearch--process)
    (let ((ev (string-trim event))
          (pending csearch--pending)
          (partial csearch--output))
      (setq csearch--process nil
            csearch--callback nil
            csearch--pending nil)
      (csearch--cancel-timer)
      (unless (member ev '("finished" "deleted"))
        (csearch--report
         (format "csearch: daemon %s%s" ev
                 (if pending (format " while running: %s" pending) ""))
         partial t)))))

(defun csearch--filter (_proc output)
  "Accumulate OUTPUT; fire callback when --END-- arrives."
  (setq csearch--output (concat csearch--output output))
  (let ((end-pos (string-match "--END--\n" csearch--output)))
    (when end-pos
      (let ((text (substring csearch--output 0 end-pos))
            (cb csearch--callback))
        (csearch--cancel-timer)
        (setq csearch--output (substring csearch--output (match-end 0))
              csearch--callback nil
              csearch--pending nil)
        (when cb
          (condition-case err (funcall cb text)
            (error (csearch--report
                    (format "csearch: %s" (error-message-string err))
                    text t))))))))

(defun csearch--timeout (command)
  "Give up on COMMAND after `csearch-timeout' seconds and say so."
  (setq csearch--timer nil)
  (when csearch--callback
    (setq csearch--callback nil
          csearch--pending nil)
    (csearch--report
     (format "csearch: no response to `%s' after %s seconds"
             command csearch-timeout)
     csearch--output t)))

(defun csearch--send (command callback)
  "Send COMMAND string to the daemon; call CALLBACK with response body."
  (let ((proc (csearch--ensure)))
    (csearch--cancel-timer)
    (setq csearch--output ""
          csearch--callback callback
          csearch--pending command
          csearch--timer (run-at-time csearch-timeout nil
                                      #'csearch--timeout command))
    (condition-case err
        (process-send-string proc (concat command "\n"))
      (error
       (csearch--cancel-timer)
       (setq csearch--callback nil
             csearch--pending nil)
       (csearch--report (format "csearch: cannot reach the daemon (%s)"
                                (error-message-string err))
                        csearch--output t)))))

(defun csearch-diagnose ()
  "Show how csearch is configured and what the daemon sees from its cwd."
  (interactive)
  (condition-case err
      (csearch--send "STATUS"
                     (lambda (text) (csearch--report "csearch status" text)))
    (error (csearch--report (format "csearch: %s" (error-message-string err))
                            nil t))))


;;; Displaying results ------------------------------------------------------

(defun csearch--short-path (file)
  "Return the last two path components (parent/filename) of FILE."
  (let ((name (file-name-nondirectory file))
        (dir  (file-name-directory file)))
    (if dir
        (concat (file-name-nondirectory (directory-file-name dir)) "/" name)
      name)))

(defun csearch--abs (file)
  "Resolve FILE to an absolute path, relative to the daemon directory."
  (if (file-name-absolute-p file) file
    (expand-file-name file csearch--directory)))

(defun csearch--common-prefix-length (a b)
  "Return the number of leading characters shared by strings A and B."
  (let ((n (min (length a) (length b)))
        (i 0))
    (while (and (< i n) (eq (aref a i) (aref b i)))
      (setq i (1+ i)))
    i))

(defun csearch--closeness (file)
  "Score FILE by path closeness to `csearch--origin' (higher is closer)."
  (if csearch--origin
      (csearch--common-prefix-length csearch--origin (csearch--abs file))
    0))

(defun csearch--note-origin ()
  "Record the current buffer's file as the origin for result sorting.
Only updates when the current buffer visits a file, so searches
re-issued from the *csearch* buffer keep the previous origin."
  (when buffer-file-name
    (setq csearch--origin (expand-file-name buffer-file-name))))

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

(cl-defun csearch--display (text)
  "Parse TEXT (file:line:content lines) and populate *csearch* buffer.
Results are displayed in a flat grep-like format with aligned columns."
  (let ((buf (get-buffer-create "*csearch*"))
        (raw-lines (split-string text "\n" t))
        (entries nil)
        (errors nil)
        (max-path-len 0))
    ;; First pass: parse lines, compute max short-path length.
    (dolist (line raw-lines)
      (if (string-match "\\`\\(ERROR\\|WARN\\|OK\\)\\b" line)
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
    (setq entries (nreverse entries)
          errors  (nreverse errors))
    ;; A failed search has no hits to show; send the reason to the error
    ;; buffer, where it comes with the paths csearch is actually using.
    (when (and (null entries) (csearch--errors text))
      (csearch--report (concat "csearch: " (car (csearch--errors text)))
                       text t)
      (cl-return-from csearch--display))
    ;; Sort by path closeness to the originating file.  `sort' is a
    ;; stable merge sort, so files of equal closeness keep their
    ;; original archive order.  Closeness is cached per path because
    ;; many entries share the same file.
    (when csearch--origin
      (let ((cache (make-hash-table :test 'equal)))
        (setq entries
              (sort entries
                    (lambda (e1 e2)
                      (let ((c1 (or (gethash (nth 0 e1) cache)
                                    (puthash (nth 0 e1)
                                             (csearch--closeness (nth 0 e1))
                                             cache)))
                            (c2 (or (gethash (nth 0 e2) cache)
                                    (puthash (nth 0 e2)
                                             (csearch--closeness (nth 0 e2))
                                             cache))))
                        (> c1 c2)))))))
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
      (find-file-other-window (csearch--abs file))
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
        (find-file-other-window (csearch--abs file))
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
  (csearch--note-origin)
  (csearch--send (concat "SEARCH " pattern)
                 #'csearch--display))

(defun csearch-symbol (symbol)
  "Search for SYMBOL with word boundaries."
  (interactive (list (csearch--read "Find symbol")))
  (csearch--note-origin)
  (csearch--send (format "SEARCH \\b%s\\b" (replace-regexp-in-string
                                              "[\\\\.*+?^${}()|\\[\\]]"
                                              "\\\\\\&" symbol))
                 #'csearch--display))

(defun csearch-text (text)
  "Literal (fixed-string) search for TEXT."
  (interactive (list (csearch--read "Find text")))
  (csearch--note-origin)
  (csearch--send (concat "SEARCH -F " text)
                 #'csearch--display))

(defun csearch--build-done (text)
  "Report the outcome of a rebuild described by TEXT."
  (let ((errors (csearch--errors text))
        (summary (car (seq-filter (lambda (l) (string-prefix-p "OK " l))
                                  (split-string text "\n" t)))))
    (cond
     (errors (csearch--report (concat "csearch: rebuild failed -- "
                                      (car errors))
                              text t))
     (summary
      (if (string-match-p "^WARN" text)
          ;; Skipped files are worth recording but not worth a popped
          ;; window on every rebuild -- stale cscope.files entries are normal.
          (csearch--report (concat "csearch: " (substring summary 3))
                           text nil t)
        (message "csearch: %s" (substring summary 3))))
     (t (csearch--report "csearch: rebuild gave no confirmation" text t)))))

(defun csearch-build ()
  "Rebuild the source archive from `csearch-files-name' in the project root.
The file list is resolved here and passed to the daemon as an absolute
path, so the rebuild does not depend on the directory the daemon happens
to have been started in."
  (interactive)
  (let* ((root (csearch--root t))
         (flist (expand-file-name csearch-files-name root)))
    (unless (file-readable-p flist)
      (csearch--report
       (format "csearch: cannot rebuild, no %s" csearch-files-name)
       (format "Expected the file list at\n  %s\n\nEither point csearch at the \
right project:\n\n    M-x csearch-set-root RET /path/to/project/ RET\n\n\
or create that file list." flist)
       t)
      (user-error "csearch: no %s" flist))
    (message "csearch: rebuilding from %s ..." flist)
    (csearch--send (concat "REBUILD " flist) #'csearch--build-done)))

;;; Global keybindings ------------------------------------------------------

(provide 'csearch)
;;; csearch.el ends here
