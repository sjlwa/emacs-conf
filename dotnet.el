(defun eshell/dotnet (&rest args)
  (interactive)
  (compile (concat "dotnet " (string-join args " "))))

(defun csharp-init-modes ()
  (flycheck-mode)
  (omnisharp-mode)
  (eglot-ensure)
  (breadcrumb-mode))

(defun csharp-init-config ()
  ;; (load-file "~/dev/sharper/sharper.el")
  (c-set-style "ellemtel")
  (setq indent-tabs-mode nil
        c-syntactic-indentation t
        c-basic-offset 4
        truncate-lines t
        tab-width 4
        evil-shift-width 4))

(defun load-lang--csharp ()
  "Loads the required configuration for csharp mode"
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

  (if (bound-and-true-p company-mode)
      (add-to-list 'company-backends 'company-omnisharp))

  (setq lsp-sonarlint-omnisharp-repository-url
        "https://github.com/SonarSource/sonarlint-omnisharp")
  (setq lsp-sonarlint-omnisharp-enabled t)

  (setq omnisharp-expected-server-version "1.39.12")
  (setq lsp-csharp-server-args
          (list "--languageserver" "--hostPID" (format "%d" (emacs-pid))))

  (add-hook 'csharp-mode-hook 'csharp-init-modes)
  (add-hook 'csharp-mode-hook 'csharp-init-config)
  (msg "Mode config loaded: chsarp-mode"))
