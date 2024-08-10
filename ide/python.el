;;(use-package lsp-pyright :ensure t)
(use-package python-ts-mode
  :mode (("\\.py\\'" . python-ts-mode))
  :hook ((python-ts-mode . lsp)
		 (python-ts-mode . company-mode))
  :config
  (setq lsp-pyright-typechecking-mode "strict"))
