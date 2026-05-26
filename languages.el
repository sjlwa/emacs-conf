;;; languages --- programming languages configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; -*- lexical-binding: t -*-

(require 'treesit-auto)

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")))

(load "~/dev/emacs-conf/major-modes/c-cpp.el" nil t)
(load "~/dev/emacs-conf/major-modes/dart.el" nil t)
(load "~/dev/emacs-conf/major-modes/dotnet.el" nil t)
(load "~/dev/emacs-conf/major-modes/golang.el" nil t)
(load "~/dev/emacs-conf/major-modes/java.el" nil t)
(load "~/dev/emacs-conf/major-modes/js-ts.el" nil t)
(load "~/dev/emacs-conf/major-modes/markdown.el" nil t)
;; (load "~/dev/emacs-conf/major-modes/makefile.el" nil t)
(load "~/dev/emacs-conf/major-modes/php.el" nil t)
(load "~/dev/emacs-conf/major-modes/plantuml.el" nil t)
(load "~/dev/emacs-conf/major-modes/python.el" nil t)
(load "~/dev/emacs-conf/major-modes/web.el" nil t)
(load "~/dev/emacs-conf/major-modes/zig.el" nil t)

(add-to-list 'auto-mode-alist '("\\.tf?\\'" . hcl-mode))
(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-ts-mode))

;; (if (require 'rainbow-delimiters nil 'noerror)
;;     (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

(provide 'languages)

;;; languages.el ends here
