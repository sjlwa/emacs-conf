;;; -*- lexical-binding: t -*-

(defun load-lang--zig ()
  (add-to-list 'treesit-language-source-alist
               '(zig "https://github.com/tree-sitter/zig-tree-sitter"))

  (add-to-list 'auto-mode-alist '("\\.zig?\\'" . zig-mode))

  (add-hook 'zig-mode-hook 'eglot-ensure)
  (add-hook 'zig-mode-hook 'breadcrumb-mode)
  (add-hook 'zig-mode-hook 'global-company-mode))
