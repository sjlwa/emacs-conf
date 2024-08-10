(use-package go-ts-mode
  :mode (("\\.go\\'" . go-ts-mode)
		 ("\\.mod\\'" . go-mod-ts-mode))
  :hook ((go-ts-mode . company-mode)
		(go-ts-mode . eglot-ensure)
		(go-ts-mode . global-flycheck-mode)))
