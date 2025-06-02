(defun sjlwa/find-file ()
  (interactive)
  (if (not (eq (vc-root-dir) nil))
      (call-interactively 'projectile-find-file)
    (call-interactively 'ido-find-file)))

(defun sjlwa/toggle-mode-line-based-on-mode ()
  "Toggle the visibility of the mode line based on the buffer's mode."
  (if (and (daemonp) (derived-mode-p 'eshell-mode))
      (setq mode-line-format nil)
    (setq mode-line-format (default-value 'mode-line-format)))
  (force-mode-line-update))

(defun sjlwa/kill-buffer-tab ()
  "Kills the current buffer and tab."
  (interactive)
  (kill-buffer (current-buffer))
  (tab-close))

(defun select-tab-by-number (num)
  "Select tab by number, treating 0 as tab 10."
  (interactive)
  (tab-bar-select-tab (if (zerop num) 10 num)))

(defun sjlwa/esc-esc-esc () (interactive)
  (if (minibufferp) (keyboard-escape-quit)))

(defun emacs-detach ()
  (let ((process-connection-type nil)) ; Use a pipe instead of a pty 
    (start-process
     "detached-emacs"                  ; Process name
     nil                               ; Buffer (nil means no buffer)
     "sh"                              ; Shell
     "-c"                              ; Run command
     (concat "nohup emacs " args " > /dev/null 2>&1 & disown"))))

(defun ee (&optional args)
  "Starts a new detached Emacs instance."
  (interactive)

  (if (eq major-mode 'eshell-mode)
      (find-file args)
    (emacs-detach)))

(defun sjlwa/tab ()
  "Tabs or Expands Emmet abbreviations in Web Mode on tab press."
  (interactive)
  (let ((last-char-before-nil
         (not (or (eq (char-before) 9) ;; tab
                  (eq (char-before) 10) ;; new line
                  (eq (char-before) 32))))) ;; space
    (cond ((and last-char-before-nil (eq major-mode 'web-mode))
           (emmet-expand-line nil)))))

(defun format@ ()
  (interactive)
  (lsp-format-buffer))

(defun sjlwa/ps-kill ()
  (require 's)
  "Search and kill process"
  (interactive)

  (sjlwa/resize-minibuffer-full-window)

  (with-temp-buffer
    (call-process "ps" nil t nil "-eo" "user,pid,args")
    (let* ((ps (completing-read "kill -9 ? " (s-split "\n" (buffer-string))))
           (ps-cols (split-string ps))
           (pid (nth 1 ps-cols))
           (pname (last ps-cols))
           (confirm (completing-read (message "Are you sure to kill %S? " pname) '("No" "Yes"))))

      (when (string= confirm "Yes")
        (call-process "kill" nil nil nil "-9" pid)
        (print pid)))))

(defalias 'pskill 'sjlwa/ps-kill)

(defun sjlwa/cpwd ()
  "Copy the current directory path"
  (interactive)
  (let ((dir default-directory))
    (with-temp-buffer
      (insert dir)
      (call-process-region (point-min) (point-max) "xclip" nil nil nil "-sel" "clip")
      (message "Copied to clipboard: %s" dir))))

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

(defalias 'open 'find-file)
