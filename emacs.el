(setq emacs-repo (file-name-directory (file-chase-links "~/.emacs")))
(load (concat emacs-repo "initializers.el"))
(init-startup)
(set-default-general-directories)

(if window-system
    (sjlwa/window-system))

(interactivity-modes-enable/configure)
(interactivity-modes-set/configure-vars)
(org-mode-define-config)

(load-config-file "fuzzy.el")
(load-config-file "packs.el")
(load-config-file "ide.el")
(load-config-file "langs.el")
(load-config-file "commands.el")
(load-config-file "eshell.el")
(load-config-file "bindings.el")

(eshell-define-init)
(esup-define-init)
