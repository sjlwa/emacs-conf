;; -*- lexical-binding: t; -*-

;; temporarily increase `gc-cons-hold' when loading to speed up startup.
(setq gc-cons-threshold most-positive-fixnum
      read-process-output-max (* 1024 1024))

(add-hook 'after-init-hook
          #'(lambda ()
              (setq gc-cons-threshold 800000
                    read-process-output-max 65536)))

(setenv "LSP_USE_PLISTS" "true")

(add-to-list 'load-path "~/dev/emacs-conf")
(add-to-list 'load-path "~/dev/emacs-conf/preload/")
(add-to-list 'load-path "~/dev/emacs-conf/major-modes/")

(require 'packages)
(require 'initializers)
(require 'functions)
(require 'ifunctions)
(require 'keymaps)
(require 'icomplete-extra)

(init-config)

;; (setq inhibit-messages t)
;; (load "~/dev/emacs-conf/major-modes/image.el" nil inhibit-messages)

;;; early-init.el ends here.
