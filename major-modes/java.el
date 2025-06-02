;;; -*- lexical-binding: t -*-

(defun load-lang--java ()
  (append treesit-language-source-alist
          '(java "https://github.com/tree-sitter/tree-sitter-java")
          '(kotlin "https://github.com/tree-sitter/kotlin-tree-sitter"))

  (add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))
  (setq lsp-java-java-path "/usr/lib/jvm/java-17-openjdk/bin/java")

  ;; (setq lsp-java-configuration-runtimes
  ;;       '[(:name "JavaSE-1.8" :path "/usr/lib/jvm/java-8-openjdk")
  ;;         (:name "JavaSE-17" :path "/usr/lib/jvm/java-17-openjdk" :default t)])

  (add-hook 'java-ts-mode-hook 'lsp)
  (add-hook 'java-ts-mode-hook 'yas-global-mode)

  (bye-buffers-add-inbetween '("*jdtls")))
