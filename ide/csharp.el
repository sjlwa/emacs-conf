(eval-after-load
 'company
 '(add-to-list 'company-backends 'company-omnisharp))

(use-package csharp-mode
  :mode (("\\.cs\\'" . csharp-mode))
  :hook ((omnisharp-start-omnisharp-server)
         (csharp-mode . lsp)
         (csharp-mode  . flycheck-mode)
         (csharp-mode . omnisharp-mode)
         (csharp-mode  . company-mode))

  :config ((setq indent-tabs-mode nil)
           (setq c-syntactic-indentation t)
           (c-set-style "ellemtel")
           (setq c-basic-offset 4)
           (setq truncate-lines t)
           (setq tab-width 4)
           (setq evil-shift-width 4)))

(load-file "~/dev/sharper/sharper.el")

