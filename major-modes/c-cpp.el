;;; -*- lexical-binding: t -*-

(defun load-language-config--c-cpp ()
  (append treesit-language-source-alist
          '(c "https://github.com/tree-sitter/tree-sitter-c")
          '(cmake "https://github.com/uyha/tree-sitter-cmake")
          '(make "https://github.com/alemuller/tree-sitter-make"))

  (add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c-ts-mode))

  (with-eval-after-load 'eglot
    (add-to-list
     'eglot-server-programs
     `(c-ts-mode . ("clangd"))))

  (add-hook 'c-ts-mode-hook 'eglot-ensure)
  (add-hook 'c-ts-mode-hook 'global-company-mode)

  (bye-buffers-add-inbetween '("clang-error" "clang-output" "clangd")))

;; (defun eshell/make (&rest args)
;;   (interactive)
;;   (eshell-call-command-as-compilation "make" args))

;; (defalias 'make 'make-compile)

(load-language-config--c-cpp)
