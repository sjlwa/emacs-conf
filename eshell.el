(defalias 'terminal 'eshell)

(defun eshell/clear ()
  (interactive) (eshell/clear-scrollback))

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

(defun sjlwa/eshell-tab-exit-close ()
  (interactive)
  (if (eq major-mode 'eshell-mode)
      (progn
        (kill-buffer (current-buffer))
        (tab-close))))

(defun history ()
  (interactive)
  (eshell-list-history)
  nil)

(defun eshell-init-setup ()
  (company-mode -1)
  (setq esh-autosuggest-use-company-map t)
  (esh-autosuggest-mode)
  (eshell-syntax-highlighting-global-mode)
  (setq eshell-hist-ignoredups t
        eshell-visual-commands (append eshell-visual-commands
                                       '("git"
                                         "dotnet"
                                         "ps"
                                         "docker"
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

(defun eshell-init-from-desktop ()
  (fringe-mode 0)
  (load-theme 'wheatgrass)
  (if (eshell-is-running)
      (sjlwa/open-eshell-tab-new)
    (eshell)))




;; TODO: custom eshell prompt
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
