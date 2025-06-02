;;; -*- lexical-binding: t -*-

(defun load-lang--javascript ()
  (add-to-list 'treesit-language-source-alist
               '(javascript "https://github.com/tree-sitter/tree-sitter-javascript"
                            "master" "src"))

  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.mjs\\'" . js2-mode))

  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'js2-mode-hook 'lsp-deferred)

  (bye-buffers-add-inbetween '("*jsts")))

(defun load-lang--typescript ()

  (append treesit-language-source-alist
          '(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          '(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
  
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))

  (add-hook 'typescript-ts-mode-hook 'lsp)
  (add-hook 'tsx-ts-mode-hook 'lsp)
  (add-hook 'tsx-ts-mode-hook
          (lambda ()
            (setq-local comment-start "{/* ")
            (setq-local comment-end   " */}")))

  (bye-buffers-add-inbetween '("ts-ls")))

(defun language-configure-astro ()
  (add-to-list 'treesit-language-source-alist
               '(astro "https://github.com/virchau13/tree-sitter-astro"))

  (add-to-list 'auto-mode-alist '("\\.astro\\'" . astro-ts-mode))
  (add-hook 'astro-ts-mode-hook
            '(lambda ()
               (setq lsp-disabled-clients '(eslint))
               (lsp)))

  (bye-buffers-add-inbetween '("astro-ls")))

(load-lang--javascript)
(load-lang--typescript)
(language-configure-astro)
