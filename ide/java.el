(use-package lsp-java
  :ensure
  :mode (("\\.java\\'" . java-ts-mode))
  :hook (java-ts-mode . lsp)
  :config (yas-global-mode))
