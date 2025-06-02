(defalias 'terminal 'eshell)

(defun eshell/clear ()
  (interactive) (eshell/clear-scrollback))

(defun sjlwa/open-eshell-tab-new ()
  "Directly open a new tab with eshell mode enabled"
  (interactive)
  (let ((default-directory "~"))
    (tab-bar-new-tab)
    (eshell t)))

(defun history ()
  (interactive)
  (eshell-list-history)
  nil)

(defun read-ext-shell-env-vars (shell)
  "Read the external shell environment variables."
  (with-temp-buffer
    (let ((command (cond
                    ((string-equal shell "/bin/zsh") ". ~/.zshrc && env")
                    ((string-equal shell "/bin/bash") ". ~/.bashrc && env")
                    (t "env"))))
      (when (= 0 (call-process shell nil t nil "-c" command))
        (split-string (buffer-string) "\n" t)))))

(defun use-ext-shell-env-vars (shell)
  "Set Emacs environment variables to match an external shell's environment variables."
  (setq process-environment (read-ext-shell-env-vars shell)))

;; (setq shell-file-name "~/.zshrc")
;; (setq shell-command-switch "-ic")
;; (use-ext-shell-env-vars "/bin/zsh")

(defun eshell-load-keymap ()
  ;; (define-key esh-autosuggest-activemde-map [tab] 'esh-autosuggest-complete-word)
  (define-key eshell-mode-map (kbd "C-w") 'ryanmarcus/backward-kill-word)
  (define-key eshell-mode-map (kbd "C-d") 'sjlwa/kill-buffer-tab)
  (define-key eshell-mode-map (kbd "C-S-t") 'sjlwa/open-eshell-tab-new))

(defun eshell-init-setup ()
  (eshell-load-keymap)
  (company-mode -1)
  ;; (setq esh-autosuggest-use-company-map t)
  (esh-autosuggest-mode)
  (eshell-syntax-highlighting-global-mode)
  (setq eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t
        eshell-visual-commands (append eshell-visual-commands
                                       '("git"
                                         "ps"
                                         "history"))))

(defun eshell-define-init ()
  ;;(setq eshell-prompt-function 'sjlwa-eshell-prompt)
  ;;(setq eshell-highlight-prompt nil)
  (add-hook 'eshell-mode-hook 'eshell-init-setup)
  (add-hook 'after-change-major-mode-hook 'sjlwa/toggle-mode-line-based-on-mode))

(defun eshell-is-running ()
  "Check if there are buffers with names starting with *eshell*"
  (cl-remove-if-not
   (lambda (buf)
     (string-prefix-p "*eshell*" (buffer-name buf)))
   (buffer-list)))

(defun eshell-init-after-server ()
  (fringe-mode 0)
  (load-theme 'modus-vivendi t nil)
  ;;(load-theme 'ef-trio-dark t nil)
  ;; (if (eshell-is-running)
  ;;     (sjlwa/open-eshell-tab-new)
  ;;   (eshell))
  (eshell))

(defun eshell-init-after-server-check ()
  (if (daemonp)
      (eshell-init-after-server)))

(add-hook 'server-after-make-frame-hook 'eshell-init-after-server-check)

(defun sjlwa-eshell-propmt-symbol ()
  (if (= (user-uid) 0) "#" "$"))

(defun sjlwa-eshell-prompt ()
  (with-face
   (concat
    (abbreviate-file-name (eshell/pwd))
    " "
    (sjlwa-eshell-propmt-symbol)
    " ")
   :foreground "thistle"
   :weight "ultra-bold"))

;; (setq eshell-prompt-function 'sjlwa-eshell-prompt)
