;;; -*- lexical-binding: t -*-

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(load "~/dev/emacs-conf/major-modes/c-cpp.el" nil inhibit-messages)
(load "~/dev/emacs-conf/major-modes/dart.el" nil inhibit-messages)
(load "~/dev/emacs-conf/major-modes/dotnet.el" nil inhibit-messages)
(load "~/dev/emacs-conf/major-modes/golang.el" nil inhibit-messages)
(load "~/dev/emacs-conf/major-modes/java.el" nil inhibit-messages)
(load "~/dev/emacs-conf/major-modes/js-ts.el" nil inhibit-messages)
(load "~/dev/emacs-conf/major-modes/markdown.el" nil inhibit-messages)
(load "~/dev/emacs-conf/major-modes/php.el" nil inhibit-messages)
(load "~/dev/emacs-conf/major-modes/python.el" nil inhibit-messages)
(load "~/dev/emacs-conf/major-modes/web.el" nil inhibit-messages)
(load "~/dev/emacs-conf/major-modes/zig.el" nil inhibit-messages)

(add-to-list 'auto-mode-alist '("\\.tf?\\'" . hcl-mode))

(if (require 'company nil 'noerror)
    (add-hook 'prog-mode-hook 'global-company-mode))

(if (require 'rainbow-delimiters nil 'noerror)
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

