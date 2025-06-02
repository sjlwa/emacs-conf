;;; -*- lexical-binding: t -*-

(defun load-lang--golang ()
  (append treesit-language-source-alist
          '(go "https://github.com/tree-sitter/tree-sitter-go")
          '(gomod "https://github.com/camdencheek/tree-sitter-go-mod"))

  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.mod\\'" . go-mod-ts-mode))

  ;; (setq lsp-go-analyses '((shadow . t)
  ;;                         (simplifycompositelit . :json-false)))

  (add-hook 'go-ts-mode-hook 'global-flycheck-mode)
  (add-hook 'go-ts-mode-hook 'lsp-deferred)
  
  (bye-buffers-add-inbetween '("gopls")))
