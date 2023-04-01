(defvar hide-lines-last-buff nil "List of project files.")
(defvar hide-lines-invisible-areas () "List of invisible overlays used by hidelines")

(defun hide-lines-not-matching (search-text)
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (setq hide-buff (replace-regexp-in-string "\\([(){}|]\\)" "\\\\\\1" search-text))
  (setq hide-lines-last-buff hide-buff)
  (print hide-buff)
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
  (interactive)
  (mapc (lambda (overlay) (delete-overlay overlay)) 
        hide-lines-invisible-areas)
  (setq hide-lines-invisible-areas ()))


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

(defun hide-function (func)
  (setq hide-mode nil)
  (setq hide-lines-column (current-column))

  (let ((selected-text (get-selected-text)))
    (progn
      (if (and selected-text (not (string= "" selected-text)))
          (setq hide-lines-text selected-text))))

  ;(if (or (not hide-lines-text) (string= "" hide-lines-text))
  ;    (setq hide-lines-text (symbol-at symbol-regexp)))

  (if (or (not hide-lines-text) (string= "" hide-lines-text))
      (setq hide-lines-text (read-string "hide-not-matching:")))

  (if (not (string= "" hide-lines-text))
      (progn
        (funcall func hide-lines-text)
        (move-to-column hide-lines-column t)
        (keyboard-quit)))
  )

(defun hide-not-matching ()
  (interactive)
  (hide-function 'hide-lines-not-matching))

(defun hide-matching ()
  (interactive)
  (hide-function 'hide-lines-matching))

(defun rehide-lines ()
  (interactive)
  (if hide-mode
      (hide-lines-matching hide-lines-text)
    (hide-lines-not-matching hide-lines-text))
  (move-to-column hide-lines-column t)
  (keyboard-quit))


(global-set-key (kbd "M-<up>") 'hide-not-matching)
(global-set-key (kbd "M-<down>") 'hide-lines-show-all)

(provide 'hide)