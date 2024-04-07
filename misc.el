(defun get-def-dir-status-cmd ()
  (format "git -C %s status 2>/dev/null" default-directory))


(defun current-git-branch ()
  (let ((branch
         (ignore-errors
           (shell-command-to-string "git branch --show-current 2> /dev/null"))))
    (replace-regexp-in-string "\n" "" branch)))


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
    (backward-kill-word 1)))


(defun eshell/clear ()
  (interactive) (eshell/clear-scrollback))

(defun sjlwa/ctrl_w ()
  (interactive)
  (if (string= (buffer-name) "*eshell*")
      (backward-kill-word 1)
    (kill-buffer)))

(defun sjlwa/esc-esc-esc () (interactive)
  (if (minibufferp) (keyboard-escape-quit)))

(defun with-face (str &rest face-plist)
  (propertize str 'face face-plist))

(defun sjlwa-eshell-prompt ()
  (concat
   (with-face (abbreviate-file-name (eshell/pwd))
              :background "#162012" :foreground "#deb")

   " "

   (with-face (let ((branch (current-git-branch)))
                (if (string= branch "")
                    ""
                  (concat branch " ")))
              :foreground "#317EAAAA1111")

     (if (= (user-uid) 0)
         (with-face "#" :background "black" :foreground "#e42")
       (with-face "$" :foreground "YellowGreen"))
     " "))
