;;; package --- initializers -*- lexical-binding: t -*-
;;; Code:

(defun init-config ()
  ;; Startup behaviour
  (setq inhibit-startup-screen t
	      inhibit-startup-message t
	      initial-scratch-message (format "Emacs %s - Hi" emacs-version)
        inhibit-compacting-font-caches t
        warning-minimum-level :emergency
        vc-follow-symlinks t
        initial-major-mode 'fundamental-mode)

  (defvar default-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)

  ;; misc
  (setq tooltip-delay 0.1
        eldoc-idle-delay 0.1
        auth-source-save-behavior nil
        package-enable-at-startup t
        backup-directory-alist '(("." . "~/.emacs.d/backups"))
        auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-saved-files/" t)))

  ;; Editor defaults
  (setq-default overwrite-mode nil
                indent-tabs-mode nil
                tab-width 2)

  (put 'upcase-region 'disabled nil)

  (add-hook 'overwrite-mode-hook (lambda () (setq overwrite-mode nil))))

(defun set-default-window/frame-config ()
    "Sets the default frame (Use in case not loading from .Xresources)."
    (setq default-frame-alist
          '((width . 90) (height . 35)
			(font . "Iosevka-16")
            (tool-bar-lines . 0)
            (menu-bar-lines . 0)
			(vertical-scroll-bars . nil)))
    (blink-cursor-mode -1))

(defun init-interactive-modes ()
  "Enable the modes required for a pleasant interactivity."
  (menu-bar-mode -1)
  (cua-mode 1)
  (xterm-mouse-mode 1)
  (column-number-mode t)
  (delete-selection-mode +1)
  (global-goto-address-mode +1)
  (electric-pair-mode t)
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  (fset 'yes-or-no-p 'y-or-n-p)
  (add-to-list 'same-window-buffer-names "*compilation*")
  (pixel-scroll-precision-mode 1) ;; Doesn't work when lsp-mode is active??
  )

(provide 'initializers)

;;; initializers.el ends here
