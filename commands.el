(defun current-git-branch ()
  "Get the current branch name of the current repository."
  (with-temp-buffer
    (when (= 0 (call-process "git" nil t nil "branch" "--show-current"))
      (string-trim (buffer-string)))))

(defun isrepo ()
  "Check whether current directory is a git repository"
  (zerop
   (call-process
    "git" nil nil nil
    "-C"
    default-directory
    "rev-parse"
    "--is-inside-work-tree")))

(defun sjlwa/find-file ()
  (interactive)
  (if (isrepo)
      (call-interactively 'projectile-find-file)
    (call-interactively 'ido-find-file)))

(defun sjlwa/cpwd ()
  "Copy the current directory path"
  (interactive)
  (let ((dir default-directory))
    (with-temp-buffer
      (insert dir)
      (call-process-region (point-min) (point-max) "xclip" nil nil nil "-sel" "clip")
      (message "Copied to clipboard: %s" dir))))

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

(defun sjlwa/kill-buffer-tab ()
  "Kills the current buffer and tab."
  (interactive)
  (kill-buffer (current-buffer))
  (tab-close))

(defun sjlwa/esc-esc-esc () (interactive)
  (if (minibufferp) (keyboard-escape-quit)))

(defun sjlwa/select-tab-by-number (num)
  "Select tab by number, treating 0 as tab 10."
  (interactive)
  (tab-bar-select-tab (if (zerop num) 10 num)))

;; (defun sjlwa/select-tab-by-number__event-handler ()
;;   (interactive)
;;   (let ((event (read-event)))
;;     (when (and (eq (event-modifier event) 'meta)
;;                (integerp (event-argument event 0)))
;;       (let ((num (1- (event-argument event 0))))
;;         (sjlwa/select-tab-by-number num)))))

(defun sjlwa/toggle-mode-line-based-on-mode ()
  "Toggle the visibility of the mode line based on the buffer's mode."
  (if (derived-mode-p 'eshell-mode)
      (setq mode-line-format nil)
    (setq mode-line-format (default-value 'mode-line-format)))
  (force-mode-line-update))

(defun with-face (str &rest face-plist)
  (propertize str 'face face-plist))

(defun my-icomplete-force-complete ()
  "Force completion of the current icomplete candidate."
  (interactive)
  (when (bound-and-true-p icomplete-mode)
    (let ((icomplete-prospects-height 1))
      (icomplete-force-complete))))

(defun set-org-mode-tasks-status ()
  (setq org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE")))
  (setq org-todo-keyword-faces
      '(("TODO" . org-todo)
        ("DOING" . org-drawer)
        ("DONE" . org-done))))

(defun ee (&optional args)
  "Starts a new detached Emacs instance."
  (interactive)
  (let ((process-connection-type nil)) ; Use a pipe instead of a pty 
    (start-process
     "detached-emacs"                  ; Process name
     nil                               ; Buffer (nil means no buffer)
     "sh"                              ; Shell
     "-c"                              ; Run command
     (concat "nohup emacs " args " > /dev/null 2>&1 & disown"))))

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

(defun sjlwa/read-file-str (filename)
  "Return the contents of a file"
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun sjlwa/eval-file (filename)
  "Evaluate the contents of FILENAME and print the result or error."
  (condition-case err
      (let ((content (sjlwa/read-file-str filename)))
        (let* ((forms (read (concat "(progn " content ")")))
               (result (eval forms)))))
    (error
     (princ (format "Error %s: %S\n" filename err)))))

(require 's)

(defun sjlwa/resize-minibuffer-full-window ()
  "Set window size of minibuffer to its max size"
  (delete-other-windows)
  (let ((minibuffer-window (minibuffer-window)))
    (select-window minibuffer-window)
    (window-resize minibuffer-window (frame-height))))

(defun sjlwa/ps-kill ()
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
