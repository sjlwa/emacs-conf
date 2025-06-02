;;; -*- lexical-binding: t -*-

(defun load-lang--php ()
  (add-to-list 'treesit-language-source-alist
               '(php "https://github.com/tree-sitter/tree-sitter-php"))

  (defun php-init () (web-mode) (lsp))

  (add-to-list 'auto-mode-alist '("\\.php?\\'" . php-init))

  (bye-buffers-add-inbetween "*iph"))
