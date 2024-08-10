(use-package c-ts-mode
  :mode (("\\.c\\'" . c-ts-mode))
  :hook (c-ts-mode . eglot-ensure))
