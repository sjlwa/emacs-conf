(setq emacs-repo "~/dev/emacs-conf/")
(load (concat emacs-repo "init-commands.el"))
(init-startup)

(if window-system
    (sjlwa/window-system))

(interactivity-modes-enable/configure)
(interactivity-modes-set/configure-vars)
(org-mode-define-config)

(sjlwa/load-fuzzy)
(sjlwa/load-packs)
(sjlwa/load-ide)
(sjlwa/load-langs)
(sjlwa/load-commands)
(sjlwa/load-bindings)

(eshell-define-config)
(esup-define-init)
