;;; -*- lexical-binding: t -*-

(defun load-lang--web ()
  (append treesit-language-source-alist
          '(css "https://github.com/tree-sitter/tree-sitter-css")
          '(html "https://github.com/tree-sitter/tree-sitter-html"))

  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  (add-to-list 'auto-mode-alist '("\\.http?\\'" . restclient-mode))

  (defun web-mode-init-config ()
    (setq web-mode-enable-auto-opening nil
          web-mode-enable-auto-pairing nil
          web-mode-enable-auto-closing nil
          web-mode-enable-auto-quoting nil
          web-mode-enable-auto-expanding nil
          web-mode-enable-auto-indentation nil))

  (add-hook 'web-mode-hook 'web-mode-init-config)
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'lsp-deferred)

  ;; (add-hook 'html-mode-hook 'emmet-mode)
  ;; (add-hook 'html-mode-hook 'lsp-deferred)
  ;; (add-hook 'html-mode-hook 'skewer-html-mode)

  ;; (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'lsp-deferred)
  (add-hook 'css-mode-hook (lambda () (setq css-indent-offset 2)))

  (bye-buffers-add-inbetween '("*css" "*html" "*eslint")))

(load-lang--web)
