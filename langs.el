(defun load-lang--c ()
  (add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
  (add-hook 'c-ts-mode-hook 'eglot-ensure))



(defun load-lang--csharp ()
  "Loads the required configuration for csharp mode"
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))
  (eval-after-load 'company '(add-to-list 'company-backends 'company-omnisharp))
  
  (defun csharp-init-modes ()
    (company-mode)
    (flycheck-mode)
    (omnisharp-mode)
    (lsp-deferred))

  (defun csharp-init-config ()
    ;; (load-file "~/dev/sharper/sharper.el")
    (c-set-style "ellemtel")
    (setq indent-tabs-mode nil
          c-syntactic-indentation t
          c-basic-offset 4
          truncate-lines t
          tab-width 4
          evil-shift-width 4))

  (add-hook 'csharp-mode-hook 'csharp-init-modes)
  (add-hook 'csharp-mode-hook 'csharp-init-config))



(defun load-lang--dart ()
  "Loads the required config for dart"
  (add-hook 'dart-mode-hook 'lsp-deferred)

  (defun dart-init-config ()
    (setq flutter-sdk-path "~/snap/flutter/common/flutter/")
    (setq lsp-dart-sdk-dir (concat flutter-sdk-path "bin/cache/dart-sdk")
          lsp-dart-flutter-sdk flutter-sdk-path))

  (eval-after-load 'dart-mode '(dart-init-config)))



(defun load-lang--golang ()
  "Loads the required config for golang"
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.mod\\'" . go-mod-ts-mode))

  (add-hook 'go-ts-mode 'company-mode)
  (add-hook 'go-ts-mode 'global-flycheck-mode)
  (add-hook 'go-ts-mode 'eglot-ensure))



(defun load-lang--java ()
  "Loads the required config for java"
  (add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))

  (add-hook 'java-ts-mode 'lsp-deferred)
  (eval-after-load 'java-ts-mode '(yas-global-mode)))



(defun load-lang--javascript ()
  "Loads the required config for javascript"
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'js2-mode-hook 'company-mode)
  (add-hook 'js2-mode-hook 'lsp-deferred))



(defun load-lang--php ()
  "Loads the required config for php"
  (defun php-init () (web-mode) (lsp))
  (add-to-list 'auto-mode-alist '("\\.php?\\'" . php-init)))



(defun load-lang--python ()
  "Loads the required config for python mode"
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))

  (add-hook 'python-ts-mode-hook 'lsp-deferred)
  (add-hook 'python-ts-mode-hook 'company-mode)

  (eval-after-load 'python-ts-mode
    '(setq lsp-pyright-typechecking-mode "strict")))



(defun load-lang--rust ()
  "Loads the required config for rust"
  ;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  ;; (add-hook 'rust-ts-mode 'company-mode)
  ;; (add-hook 'rust-ts-mode 'lsp-deferred)
  (add-hook 'rust-mode 'company-mode)
  (add-hook 'rust-mode 'lsp-deferred))



(defun load-lang--typescript ()
  "Loads the required config for typescript mode"
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

  (add-hook 'typescript-ts-mode-hook 'lsp-deferred)
  (add-hook 'typescript-ts-mode-hook 'company-mode))



(defun load-lang--web ()
  "Loads the required config for web mode"
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
