(defun init-config ()
  (setq inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-compacting-font-caches t
        file-name-handler-alist nil
        warning-minimum-level :emergency
        package-enable-at-startup nil
        vc-follow-symlinks t
        tooltip-delay 0.1
        eldoc-idle-delay 0
        indent-tabs-mode nil
        initial-major-mode 'fundamental-mode
        auth-source-save-behavior nil
        backup-directory-alist '(("." . "~/.emacs.d/backups"))
        auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-saved-files/" t)))

  (setq-default indent-tabs-mode nil
                electric-indent-inhibit t
                tab-width 4))

(defun interactivity-mode ()
  "Enable the modes required for a pleasant interactivity."
  (cua-mode 1)
  (xterm-mouse-mode 1)
  (column-number-mode t)
  (delete-selection-mode +1)
  (global-goto-address-mode +1)
  (electric-pair-mode t)
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  (menu-bar-mode -1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (add-to-list 'same-window-buffer-names "*compilation*")
  ;; (pixel-scroll-precision-mode 1) ;; Doesn't work when lsp-mode is active
  (with-eval-after-load 'diff-hl
    (add-hook 'after-init-hook 'global-diff-hl-mode)))

(defun show-init-scratch-message ()
  "Insert initial scratch message directly"
  (with-current-buffer "*scratch*"
    (let* ((message (format "Emacs %s - Hi" emacs-version))
           (width (window-total-width))
           (height (window-total-height))
           (padding-x (make-string (/ (- width (length message))) ?\s))
           (padding-y (make-string (/ (1- height)) ?\n)))
      (erase-buffer)
      (insert (concat padding-y padding-x message))
      (put-text-property (point-min) (point-max)
                         'face '(:foreground "Magenta")))))

(defun set-default-window/frame-config ()
    "Sets the default frame (Use in case not loading from .Xresources)."
    (setq default-frame-alist
          '((width . 90) (height . 35)
			(font . "Iosevka-16")
            (tool-bar-lines . 0)
            (menu-bar-lines . 0)
			(vertical-scroll-bars . nil)))
    (blink-cursor-mode -1))

(defun org-mode-define-tasks-status ()
  (setq org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE")))
  (setq org-todo-keyword-faces
      '(("TODO" . org-todo)
        ("DOING" . org-drawer)
        ("DONE" . org-done))))

(defun org-mode-define-config ()
  (with-eval-after-load 'org
    (setq org-support-shift-select t
          org-startup-indented t)
    (org-mode-define-tasks-status)
    (add-hook 'org-mode-hook #'visual-line-mode)))

(defun esup-define-init ()
  (with-eval-after-load 'esup
    (setq esup-user-init-file (file-truename "~/.emacs"))
    (setq esup-depth 0)))

(defun init-window-system ()
  (show-init-scratch-message)
  (load-theme 'manoj-dark t))

(defun init-terminal-clipboard ()
  (require 'clipetty)
  (require 'xclip)
  (global-clipetty-mode)
  (xclip-mode)
  (normal-erase-is-backspace-mode 0)
  (diff-hl-margin-mode 1))

(defun init-terminal-system ()
  (load-theme 'hc-zenburn t nil)
  (set-face-attribute 'default nil :background "#111")
  (add-hook 'after-init-hook #'init-terminal-clipboard)
  (msg "init-terminal-system done!"))
