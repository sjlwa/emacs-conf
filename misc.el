(defun get-def-dir-status-cmd ()
  (format "git -C %s status 2>/dev/null" default-directory))

(defun isrepo ()
  "Verifica si el directorio actual es un repositorio Git."
  (interactive)
  (let ((status (shell-command-to-string (get-def-dir-status-cmd))))
    (> (length status) 0)))

(defun my-find-file ()
  (interactive)
  (if (isrepo)
      (call-interactively 'projectile-find-file)
    (call-interactively 'ido-find-file)))

;; copy working directory
(defun sjlwa/cpwd ()
  (interactive)
  (let ((wd (nth 1 (s-split "Directory " (pwd)))))
    (shell-command (concat "echo " wd " | xclip -sel clip &> /dev/null"))
    (prin1 (concat "Copied: " wd))))
