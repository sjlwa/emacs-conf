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

  :config ((load-file "~/dev/sharper/sharper.el")
           (setq indent-tabs-mode nil
                 c-syntactic-indentation t
                 c-basic-offset 4
                 truncate-lines t
                 tab-width 4
                 evil-shift-width 4)
           (c-set-style "ellemtel")))

