;;; -*- lexical-binding: t -*-

(defun load-lang--rust ()
  (add-to-list 'treesit-language-source-alist
               '(rust "https://github.com/tree-sitter/tree-sitter-rust"))

  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-hook 'rust-ts-mode 'lsp-deferred)
  (add-hook 'rust-mode 'lsp))

