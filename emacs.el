(setq inhibit-messages t)
(load "~/dev/emacs-conf/icommands.el" nil inhibit-messages)
(load "~/dev/emacs-conf/initializers.el" nil inhibit-messages)

(init-startup)

(if (display-graphic-p)
    (init-window-system)
  (init-terminal-system))

(load "~/dev/emacs-conf/fuzzy.el" nil inhibit-messages)
(load "~/dev/emacs-conf/packs.el" nil inhibit-messages)
(load "~/dev/emacs-conf/ide.el" nil inhibit-messages)
(load "~/dev/emacs-conf/langs.el" nil inhibit-messages)
(load "~/dev/emacs-conf/commands.el" nil inhibit-messages)
(load "~/dev/emacs-conf/eshell.el" nil inhibit-messages)
(load "~/dev/emacs-conf/bindings.el" nil inhibit-messages)
(load "~/dev/emacs-conf/dired.el" nil inhibit-messages)

(fuzzy-mode)
(keymap-global-load)
(org-mode-define-config)
(markdown-mode-set-config)
(eshell-define-init)
(esup-define-init)
(dired-configure)

(setq auth-source-save-behavior nil)
