;;; package --- commands -*- lexical-binding: t -*-

(require 'vc-git)

(defun sjlwa/find-file ()
  (interactive)
  (if (not (eq (vc-git-root default-directory) nil))
      (call-interactively 'projectile-find-file)
    (call-interactively 'ido-find-file)))

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

(defun format-buffer ()
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (lsp-format-buffer))
  (if (bound-and-true-p eglot-mode)
      (eglot-format)))

(defun sjlwa/ps-kill ()
  "Search and kill process"
  (interactive)

  (require 's)
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
        (kill-emacs)))))

(defalias 'pskill 'sjlwa/ps-kill)

(defalias 'open 'find-file)

;; (defun org-insert-src-block (src-code-type)
;;   "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
;;   (interactive
;;    (let ((src-code-types
;;           '("emacs-lisp" "sh" "sql" "lisp" "org" "sqlite")))
;;      (list (ido-completing-read "Source code type: " src-code-types))))

;;   (progn
;;     (newline-and-indent)
;;     (insert (format "#+begin_src %s\n" src-code-type))
;;     (newline-and-indent)
;;     (insert "#+end_src\n")
;;     (previous-line 2)
;;     (org-edit-src-code)))

(defun xfce-terminal ()
  (interactive)
  (start-process "detached-xfce-terminal" nil "sh" "-c"
                 "setsid xfce4-terminal"))

(defun sjlwa/convert-line-endings (file program)
  "Convert line endings of FILE using PROGRAM.
PROGRAM should be 'unix2dos' or 'dos2unix'."
  (shell-command (format "%s %s" program (shell-quote-argument file))))

(defun sjlwa/magit-convert-line-endings (program)
  "Convert line endings of file at point in Magit using PROGRAM."
  (require 'magit)
  (when-let ((file (magit-file-at-point)))
    (let ((file-path (expand-file-name file (magit-toplevel))))
      (sjlwa/convert-line-endings file-path program)
      (magit-refresh)
      (message "Converted %s using %s" file program))))

(defun sjlwa/magit-unix2dos ()
  "Convert line endings of file at point in Magit to DOS format."
  (interactive)
  (sjlwa/magit-convert-line-endings "unix2dos"))

(defun sjlwa/magit-dos2unix ()
  "Convert line endings of file at point in Magit to Unix format."
  (interactive)
  (sjlwa/magit-convert-line-endings "dos2unix"))

(defun environment-load-file (filename)
  "Load .env FILENAME to the Emacs environment."
  (interactive
   (list (read-file-name "Select env file: " nil nil t)))
  (let ((path (shell-command-to-string
               (format "set -a; . %s; echo -n $PATH; set +a" filename))))
    (setenv "PATH" path)
    (setq exec-path (append (split-string-and-unquote path ":") exec-path))))

(defun project-compile-w-env ()
  "Compile with specified environment variables from a filename."
  (interactive)
  (call-interactively #'environment-load-file)
  (project-compile))

(defun spellcheck ()
  (interactive)
  (call-interactively #'ispell-change-dictionary)
  (flyspell-buffer)
  (ispell-buffer))

(provide 'commands)

;;; commands.el ends here
