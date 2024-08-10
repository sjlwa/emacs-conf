(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode))
  :hook ((typescript-ts-mode . lsp)
         (typescript-ts-mode . company-mode)))

;;(add-to-list 'load-path "~/.emacs.d/tsx-mode/")
;;(setq typescript-indent-level 2)
;;(add-hook 'typescript-mode-hook #'eglot-ensure)
