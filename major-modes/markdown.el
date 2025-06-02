;;; -*- lexical-binding: t -*-

(defun markdown-mode-set-config ()
  (setq markdown-command "/usr/bin/pandoc" ;; pandoc-bin
        markdown-live-preview-delete-export 'delete-on-export
        markdown-ts-url "https://github.com/tree-sitter-grammars/tree-sitter-markdown")

  (append
   treesit-language-source-alist
   '(markdown markdown-ts-url "split_parser" "tree-sitter-markdown/src")
   '(markdown-inline markdown-ts-url "split_parser" "tree-sitter-markdown-inline/src"))

  ;; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-ts-mode))
  ;;  (setq markdown-fontify-code-blocks-natively t)

  (add-hook 'markdown-mode-hook
            #'(lambda ()
                (set-face-attribute'markdown-pre-face nil :family "IosevkaExtraLight"))))

(markdown-mode-set-config)
