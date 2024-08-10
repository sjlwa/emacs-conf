(use-package emmet-mode
  :ensure t)

(use-package html-mode
  :hook ((css-mode . emmet-mode) (css-mode . lsp)))

(use-package css-mode
  :hook ((css-mode . emmet-mode) (css-mode . lsp))
  :config (setq css-indent-offset 2))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(use-package web-mode
  :hook ((web-mode. emmet-mode) (web-mode . lsp))
  :config
  ;;(setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-opening nil)
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-enable-auto-closing nil)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-auto-expanding nil)
  (setq web-mode-enable-auto-indentation nil))
