;;; -*- lexical-binding: t -*-

(add-to-list 'treesit-language-source-alist
             '(php "https://github.com/tree-sitter/tree-sitter-php"))

;; (add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . php-ts-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((php-mode php-ts-mode) . ("phpantom_lsp" "--stdio"))))

(add-to-list 'load-path "~/dev/php-xdebug/")

(add-hook 'php-ts-mode-hook
          (lambda ()
            (require 'php-xdebug nil t)
            (require 'php-xdebug-profile-ui nil t)))
