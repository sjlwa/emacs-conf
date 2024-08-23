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

(defun sjlwa/find-file ()
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
  (if (eq major-mode 'eshell-mode)
      (backward-kill-word 1)
    (kill-buffer)))

(defun sjlwa/esc-esc-esc () (interactive)
  (if (minibufferp) (keyboard-escape-quit)))

(defalias 'terminal 'eshell)

(defun sjlwa/open-eshell-tab-new ()
  "Directly open a new tab with eshell mode enabled"
  (tab-bar-new-tab)
  (let ((current-prefix-arg '(4)))
         (call-interactively 'eshell)))

(defun sjlwa/tab-new ()
  "Open eshell tab if current buffer is eshell"
  (interactive)
  (if (eq major-mode 'eshell-mode)
      (sjlwa/open-eshell-tab-new)))

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


(defun eshell-init-setup ()
  (company-mode -1)
  (setq eshell-visual-commands (append eshell-visual-commands '("git" "dotnet" "ps"))
        eshell-hist-ignoredups t)
  (setq esh-autosuggest-use-company-map t)
  (esh-autosuggest-mode))


(defun sjlwa/eshell-tab-exit-close ()
  (interactive)
  (if (eq major-mode 'eshell-mode)
      (progn
        (kill-buffer (current-buffer))
        (tab-close))))



(defun with-face (str &rest face-plist)
  (propertize str 'face face-plist))

;; (defun sjlwa-eshell-prompt ()
;;   (concat
;;    (with-face (abbreviate-file-name (eshell/pwd))
;;               :background color-prompt-pwd-bg :foreground color-prompt-pwd-fg)

;;    " "

;;    (with-face (let ((branch (current-git-branch)))
;;                 (if (string= branch "")
;;                     ""
;;                   (concat branch " ")))
;;               :foreground color-prompt-branch)

;;      (if (= (user-uid) 0)
;;          (with-face "#" :background "black" :foreground "#e42")
;;        (with-face "$" :foreground color-prompt-shebang))
;;      " "))


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


(defun sjlwa/load-packages-sources ()
  "Load the packages archives"
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (package-initialize))

;; Lazy load extra package sources
(advice-add 'list-packages :before #'sjlwa/load-packages-sources)



(defun sjlwa/install-packages ()
  (interactive)
  (sjlwa/load-packages-sources)

  (package-install 'ag)
  (package-install 'diff-hl)
  (package-install 'magit)
  (package-install 'projectile)
  (package-install 'http)
  (package-install 'expand-region)
  (package-install 'move-text)
  (package-install 'esup)

  ;; (package-install 'format-alel)

  (package-install 'company)
  (package-install 'company-quickhelp)
  (package-install 'lsp-mode)
  (package-install 'lsp-ui)
  (package-install 'tree-sitter)
  (package-install 'tree-sitter-langs)
  (package-install 'flycheck)
  (package-install 'editorconfig)
  (package-install 'yasnippet)
  (package-install 'esh-autosuggest)

  ;; Web
  (package-install 'emmet-mode)
  ;; (package-install 'html-mode) ???
  ;; (package-install 'css-mode) ???
  ;; (package-install 'web-mode) ???
  
  (print "Done."))
