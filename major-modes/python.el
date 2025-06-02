;;; -*- lexical-binding: t -*-

(defun load-lang--python ()
  (add-to-list 'treesit-language-source-alist
               '(python "https://github.com/tree-sitter/tree-sitter-python"))

  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))

  (add-hook 'python-ts-mode-hook 'lsp-deferred)
  (add-hook 'python-ts-mode-hook 'eglot)  

  (eval-after-load 'python-ts-mode
    '(setq lsp-pyright-typechecking-mode "strict"))

  (bye-buffers-add-inbetween '("pylsp" "pyright")))
