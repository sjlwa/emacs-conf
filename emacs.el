;; -*- lexical-binding: t; -*-

(if (display-graphic-p)
    (init-window-system)
  (init-terminal-system))

(load "~/dev/emacs-conf/bye-buffers.el" nil inhibit-messages)
(load "~/dev/emacs-conf/commands.el" nil inhibit-messages)
(load "~/dev/emacs-conf/major-modes/eshell.el" nil inhibit-messages)
(load "~/dev/emacs-conf/bindings.el" nil inhibit-messages)
(load "~/dev/emacs-conf/dbcli.el" nil inhibit-messages)

(init-config)
(interactivity-mode)
(keymap-global-load)
(org-mode-define-config)
(eshell-define-init)
(esup-define-init)
(dired-configure)
(bye-buffers-mode)

(load "~/dev/emacs-conf/ide.el" nil inhibit-messages)
(load "~/dev/emacs-conf/languages.el" nil inhibit-messages)
