(setq treesit-language-source-alist
      '((bash (github.com "tree-sitter" "tree-sitter-bash"))        
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (php "https://github.com/tree-sitter/tree-sitter-php")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(load "~/dev/emacs-conf/dotnet.el" nil inhibit-messages)

(defun load-lang--c ()
  (bye-buffers-add-inbetween '("clang-error" "clang-output" "clangd"))
  (add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
  (add-hook 'c-ts-mode-hook 'eglot-ensure))


(defun load-lang--dart ()
  "Loads the required config for dart"
  (add-hook 'dart-mode-hook 'lsp-deferred)
  (bye-buffers-add-inbetween '("dart_analysis" "LSP Dart"))
  (defun dart-init-config ()
    (setq flutter-sdk-path "~/snap/flutter/common/flutter/")
    (setq lsp-dart-sdk-dir (concat flutter-sdk-path "bin/cache/dart-sdk")
          lsp-dart-flutter-sdk flutter-sdk-path))

  (eval-after-load 'dart-mode '(dart-init-config)))



(defun load-lang--golang ()
  "Loads the required config for golang"
  (bye-buffers-add-inbetween '("gopls"))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.mod\\'" . go-mod-ts-mode))

  (add-hook 'go-ts-mode 'global-flycheck-mode)
  (add-hook 'go-ts-mode 'eglot-ensure))



(defun load-lang--java ()
  "Loads the required config for java"
  (add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))
  (setq lsp-java-java-path "/usr/lib/jvm/java-17-openjdk/bin/java")

  (bye-buffers-add-inbetween '("*jdtls"))

  (bye-buffers-add-inbetween "*jdtls")

  (add-hook 'java-ts-mode 'lsp-deferred)
  (eval-after-load 'java-ts-mode '(yas-global-mode)))



(defun load-lang--javascript ()
  "Loads the required config for javascript"
  (bye-buffers-add-inbetween '("*jsts"))

  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.mjs\\'" . js2-mode))

  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'js2-mode-hook 'lsp-deferred))



(defun load-lang--php ()
  "Loads the required config for php"
  (defun php-init () (web-mode) (lsp))
  (add-to-list 'auto-mode-alist '("\\.php?\\'" . php-init))
  (bye-buffers-add-inbetween "*iph")
  )



(defun load-lang--python ()
  "Loads the required config for python mode"
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))

  (add-hook 'python-ts-mode-hook 'lsp-deferred)

  (bye-buffers-add-inbetween '("pylsp" "pyright"))

  (eval-after-load 'python-ts-mode
    '(setq lsp-pyright-typechecking-mode "strict")))



(defun load-lang--rust ()
  "Loads the required config for rust"
  ;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  ;; (add-hook 'rust-ts-mode 'lsp-deferred)
  ;;(add-hook 'rust-mode 'lsp)
  )



(defun load-lang--typescript ()
  "Loads the required config for typescript mode"
  (bye-buffers-add-inbetween '("ts-ls"))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))
  (add-hook 'typescript-ts-mode-hook 'lsp)
  (add-hook 'tsx-ts-mode-hook 'lsp)
  (add-hook 'tsx-ts-mode-hook
          (lambda ()
            (setq-local comment-start "{/* ")
            (setq-local comment-end   " */}"))))



(defun load-lang--web ()
  "Loads the required config for web mode"
  (bye-buffers-add-inbetween '("*css" "*html" "*eslint"))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  (defun web-mode-init-config ()
    (setq web-mode-enable-auto-opening nil
          web-mode-enable-auto-pairing nil
          web-mode-enable-auto-closing nil
          web-mode-enable-auto-quoting nil
          web-mode-enable-auto-expanding nil
          web-mode-enable-auto-indentation nil))

  (add-hook 'web-mode-hook 'web-mode-init-config)
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'lsp-deferred)

  ;; (add-hook 'html-mode-hook 'emmet-mode)
  ;; (add-hook 'html-mode-hook 'lsp-deferred)
  ;; (add-hook 'html-mode-hook 'skewer-html-mode)

  ;; (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'lsp-deferred)
  (add-hook 'css-mode-hook (lambda () (setq css-indent-offset 2))))



(if (require 'company nil 'noerror)
    (add-hook 'prog-mode-hook 'company-mode))

(if (require 'rainbow-delimiters nil 'noerror)
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


(load-lang--c)
(load-lang--csharp)
(load-lang--dart)
(load-lang--golang)
(load-lang--java)
(load-lang--javascript)
(load-lang--php)
(load-lang--python)
(load-lang--rust)
(load-lang--typescript)
(load-lang--web)
