(defvar hide-lines-last-buff nil "List of project files.")
(defvar hide-lines-invisible-areas () "List of invisible overlays used by hidelines")
(defvar hide-lines-active nil "Non-nil while lines are hidden (overview mode).")

(defun hide-lines-not-matching (search-text)
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (setq hide-buff (replace-regexp-in-string "\\([(){}|]\\)" "\\\\\\1" search-text))
  (setq hide-lines-last-buff hide-buff)
  (save-excursion 
    (goto-char (point-min))
    (let ((start-position (point-min))
          (pos (re-search-forward hide-buff nil t)))
      (while pos
        (beginning-of-line)
        (hide-lines-add-overlay start-position (point))
        (forward-line 1)
        (setq start-position (point))
        (if (eq (point) (point-max))
            (setq pos nil)
          (setq pos (re-search-forward hide-buff nil t))))
      (hide-lines-add-overlay start-position (point-max)))))

(defun hide-lines-matching  (search-text)
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (save-excursion
    (goto-char (point-min))
    (let ((pos (re-search-forward search-text nil t))
          start-position)
      (while pos
        (beginning-of-line)
        (setq start-position (point))
        (end-of-line)
        (hide-lines-add-overlay start-position (+ 1 (point)))
        (forward-line 1)
        (if (eq (point) (point-max))
            (setq pos nil)
          (setq pos (re-search-forward search-text nil t)))))))

(defun hide-lines-add-overlay (start end)
  (let ((overlay (make-overlay start end)))
    (setq hide-lines-invisible-areas (cons overlay hide-lines-invisible-areas))
    (overlay-put overlay 'invisible 'hl)))

(defun hide-lines-show-all ()
  "Show every line again, centred on the line point is on.
Point is left where overview navigation put it, so leaving overview
mode lands on the match that was being looked at."
  (interactive)
  (mapc (lambda (overlay) (delete-overlay overlay))
        hide-lines-invisible-areas)
  (setq hide-lines-invisible-areas ())
  (setq hide-lines-active nil)
  ;; Only when this buffer is actually on screen -- `recenter' errors otherwise.
  (if (eq (current-buffer) (window-buffer (selected-window)))
      (recenter)))

(defun hide-lines-hidden-at-p (pos)
  "Non-nil if POS is covered by one of our invisible overlays."
  (let ((hidden nil))
    (dolist (o (overlays-at pos) hidden)
      (if (overlay-get o 'invisible) (setq hidden t)))))

(defun hide-lines-goto-visible ()
  "Move point off a hidden line onto the next visible one."
  (while (and (not (eobp)) (hide-lines-hidden-at-p (point)))
    (goto-char (next-overlay-change (point))))
  (beginning-of-line))



(defvar symbol-regexp "[a-zA-Z0-9_]+" "")

(defun symbol-at (regexp)
  (let ((fore-marker nil))
	(save-excursion
	  (if (or (looking-at regexp) (re-search-backward regexp nil t))
		  (progn
			(while (and (looking-at regexp) (not (bobp))) (backward-char 1))
			(if (re-search-forward regexp nil t)
				(buffer-substring (match-beginning 0) (point-marker))
			  (identity "")))
		(identity "")))))

(defun current-symbol ()
  (interactive)
  (message (symbol-at symbol-regexp)))


(defun get-selected-text ()
  (when (region-active-p)
    (let (deactivate-mark)
      (buffer-substring (region-beginning) (region-end)))))


(defvar hide-mode nil "hide matching or not")
(defvar hide-lines-column nil "column for hide-lines")
(defvar hide-lines-text "" "text for hide-lines")

(defun hide-lines-read-text (prompt)
  "Text to hide by: the active region, else PROMPT prefilled for editing.
With no region the default is the symbol point is on -- or the nearest
one behind it -- falling back to the last text used."
  (let ((selected-text (get-selected-text)))
    (if (and selected-text (not (string= "" selected-text)))
        selected-text
      (let* ((near (symbol-at symbol-regexp))
             (default (or (thing-at-point 'symbol t)
                          (and (not (string= "" near)) near)
                          hide-lines-text)))
        (read-string prompt default)))))

(defun hide-function (func prompt matching)
  ;; Re-entering overview replaces the previous keyword instead of
  ;; narrowing on top of it, so the search always sees the whole buffer.
  (if hide-lines-active (hide-lines-show-all))
  (setq hide-mode matching)
  (setq hide-lines-column (current-column))
  (setq hide-lines-text (hide-lines-read-text prompt))

  (if (not (string= "" hide-lines-text))
      (progn
        (funcall func hide-lines-text)
        (setq hide-lines-active t)
        (move-to-column hide-lines-column t)
        (deactivate-mark)))
  )

(defun hide-not-matching ()
  "Overview mode, with no prompt.  The token comes from, in order:
the active region, which becomes the new token; else `hide-lines-text'
as last used, making this the repeat key; else -- a fresh session, or
the last filter cleared -- `hide-lines-filter', the same
letter-by-letter search C-c f runs."
  (interactive)
  ;; read the region before anything below disturbs it
  (let ((selected (get-selected-text)))
    (if (and selected (not (string= "" selected)))
        (setq hide-lines-text selected)))
  (if (or (not hide-lines-text) (string= "" hide-lines-text))
      (hide-lines-filter)
    (if hide-lines-active (hide-lines-show-all))
    (setq hide-mode nil)
    (setq hide-lines-column (current-column))
    (hide-lines-not-matching hide-lines-text)
    (setq hide-lines-active t)
    ;; point may have been sitting on a line this token hides
    (hide-lines-goto-visible)
    (move-to-column hide-lines-column t)
    (deactivate-mark)
    ;; no prompt appeared, so say which token was applied
    (message "filter: %s" hide-lines-text)))

(defun hide-matching ()
  "Hide every line matching a keyword."
  (interactive)
  (hide-function 'hide-lines-matching "hide-matching: " t))

(defun rehide-lines ()
  (interactive)
  (if hide-mode
      (hide-lines-matching hide-lines-text)
    (hide-lines-not-matching hide-lines-text))
  (setq hide-lines-active t)
  (move-to-column hide-lines-column t)
  (deactivate-mark))



;;; Incremental filter -------------------------------------------------------

(defvar hide-lines-filter-target nil
  "Buffer being filtered live by `hide-lines-filter'.")

(defun hide-lines-filter-apply (text)
  "Re-hide the buffer against TEXT.
A half-typed regexp (a lone [ or \\) is not an error here -- it just
matches nothing yet, so typing is never interrupted."
  (hide-lines-show-all)
  (if (not (string= "" text))
      (condition-case nil
          (hide-lines-not-matching text)
        (error (hide-lines-show-all)))))

(defun hide-lines-filter-update ()
  "Refilter the target buffer from the text typed so far."
  (if (buffer-live-p hide-lines-filter-target)
      (let ((text (minibuffer-contents-no-properties)))
        (with-current-buffer hide-lines-filter-target
          (hide-lines-filter-apply text)))))

(defun hide-lines-filter ()
  "Hide non-matching lines as you type, letter by letter.
RET leaves the filter applied and returns point to the buffer, so you
can move around the matches; \\[keyboard-quit] restores the buffer.
Expand again with \\[hide-lines-show-all]."
  (interactive)
  (let ((hide-lines-filter-target (current-buffer))
        (origin (point))
        (column (current-column))
        (text nil))
    (condition-case nil
        (progn
          (setq text
                (minibuffer-with-setup-hook
                    (lambda ()
                      ;; buffer-local to the minibuffer, so it dies with it
                      (add-hook 'post-command-hook
                                'hide-lines-filter-update nil t))
                  (read-string "filter: ")))
          (setq hide-lines-text text)
          (setq hide-mode nil)
          (setq hide-lines-column column)
          (if (string= "" text)
              (hide-lines-show-all)
            (setq hide-lines-active t)
            (hide-lines-goto-visible)
            (move-to-column column t)
            (if (eq (current-buffer) (window-buffer (selected-window)))
                (recenter))))
      ;; C-g: put the buffer back the way it was
      (quit
       (goto-char origin)
       (hide-lines-show-all)
       (signal 'quit nil)))))


(global-set-key (kbd "M-<up>") 'hide-not-matching)
(global-set-key (kbd "M-<down>") 'hide-lines-show-all)
(global-set-key (kbd "C-c f") 'hide-lines-filter)

(provide 'hide)