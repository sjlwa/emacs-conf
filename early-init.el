;; temporarily increase `gc-cons-hold' when loading to speed up startup.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

(setq inhibit-messages t)
(load "~/dev/emacs-conf/icommands.elc" nil inhibit-messages)
(load "~/dev/emacs-conf/initializers.el" nil inhibit-messages)
(load "~/dev/emacs-conf/fuzzy.elc" nil inhibit-messages)
(load "~/dev/emacs-conf/packs.elc" nil inhibit-messages)
(load "~/dev/emacs-conf/ide.elc" nil inhibit-messages)
(load "~/dev/emacs-conf/langs.elc" nil inhibit-messages)
(load "~/dev/emacs-conf/commands.elc" nil inhibit-messages)
(load "~/dev/emacs-conf/eshell.elc" nil inhibit-messages)
(load "~/dev/emacs-conf/bindings.elc" nil inhibit-messages)
(load "~/dev/emacs-conf/dired.elc" nil inhibit-messages)

(init-config)
(interactivity-mode)
(fuzzy-mode)
(keymap-global-load)
(org-mode-define-config)
(markdown-mode-set-config)
(eshell-define-init)
(esup-define-init)
(dired-configure)