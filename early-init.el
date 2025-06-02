;; -*- lexical-binding: t; -*-

;; temporarily increase `gc-cons-hold' when loading to speed up startup.
(setq gc-cons-threshold most-positive-fixnum
      read-process-output-max (* 1024 1024))

(add-hook 'after-init-hook
          #'(lambda ()
              (setq gc-cons-threshold 800000
                    read-process-output-max 65536)))
;; (setenv "LSP_USE_PLISTS" "true")

(setq inhibit-messages t)

(load "~/dev/emacs-conf/initializers.el" nil inhibit-messages)
(load "~/dev/emacs-conf/icommands.el" nil inhibit-messages)
(load "~/dev/emacs-conf/packs.el" nil inhibit-messages)
(load "~/dev/emacs-conf/icomplete.el" nil inhibit-messages)
(load "~/dev/emacs-conf/major-modes/dired.el" nil inhibit-messages)
(load "~/dev/emacs-conf/major-modes/image.el" nil inhibit-messages)

;;; early-init.el ends here.
