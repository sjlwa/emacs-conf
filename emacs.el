;;; emacs --- emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'clipetty)
(require 'xclip)

(require 'commands)
(require 'dired-extra)
(require 'org-extra)
(require 'company-extra)
(require 'eshell-extra)
(require 'clipboard)
(require 'eglot-extra)

(require 'bye-buffers)
(require 'bye-buffers-list)
(bye-buffers-mode)

(require 'languages)


(setq esup-depth 0)
(defun esup-define-init ()
  (with-eval-after-load 'esup
    (setq esup-user-init-file (file-truename "~/.emacs"))
    (setq esup-depth 0)))


(add-hook 'after-init-hook #'init-interactive-modes)
(add-hook 'after-init-hook #'keymap-global-load)
(add-hook 'after-init-hook #'global-diff-hl-mode)

(defun start-window-system ()
  (load-theme 'nojma t))

(defun start-terminal-clipboard ()
  (global-clipetty-mode)
  (xclip-mode)
  (normal-erase-is-backspace-mode 0)
  (diff-hl-margin-mode 1))

(defun start-terminal-system ()
  (start-terminal-clipboard))

(defun start-display ()
  (make-thread (lambda ()
		 (if (display-graphic-p)
		     (start-window-system)
		   (start-terminal-system)))))

(add-hook 'after-init-hook #'start-display)

(setq magit-log-margin '(t age 40 t 12))

(provide 'emacs)

;;; emacs.el ends here
