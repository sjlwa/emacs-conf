;;; package --- ifunctions -*- lexical-binding: t -*-
;;; Code:

(defun tab-kill-buffer ()
  "Kills the current buffer and tab."
  (interactive)
  (kill-buffer (current-buffer))
  (tab-close))

(defun tab-select-by-number (num)
  "Select tab by number, treating 0 as tab 10."
  (interactive)
  (tab-bar-select-tab (if (zerop num) 10 num)))

(defun esc-esc-esc ()
  (interactive)
  (if (minibufferp) (keyboard-escape-quit)))

(defun xahlee/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun xahlee/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (xahlee/delete-word (- arg)))

(defun xahlee/delete-line ()
  "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
  (interactive)
  (delete-region
   (point)
   (progn (end-of-line 1) (point)))
  (delete-char 1))

(defun xahlee/delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position.
This command does not push text to `kill-ring'."
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))

(defun ryanmarcus/backward-kill-word ()
  "Remove all whitespace if the character behind the cursor is whitespace, otherwise remove a word."
  (interactive)
  (if (looking-back "[ \n]")
      ;; delete horizontal space before us and then check to see if we
      ;; are looking at a newline
      (progn (delete-horizontal-space 't)
             (while (looking-back "[ \n]")
               (backward-delete-char 1)))
    ;; otherwise, just do the normal kill word.
    (xahlee/backward-delete-word 1)))

(defun font-set-weight (weight)
  "Set the frame font weight based on user selection."
  (interactive
   (let ((weights '("Light" "Regular" "Medium")))
     (list (ido-completing-read "Select font weight: " weights))))
  
  (if (string-equal weight "Regular")
      (set-frame-font "Iosevka 16" nil t)
    (set-frame-font (format "Iosevka %s 16" weight) nil t)))

(defun kill-matching-buffers (sub-string)
  (dolist (buffer (buffer-list))
      (when (string-match-p (regexp-quote sub-string) (buffer-name buffer))
        (kill-buffer buffer))))

(provide 'ifunctions)

;;; ifunctions.el ends here
