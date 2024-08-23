(defun csharp-init-modes ()
  (company-mode)
  (flycheck-mode)
  (omnisharp-mode)
  ;; (omnisharp-start-omnisharp-server)
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

(eval-after-load
 'company
 '(add-to-list 'company-backends 'company-omnisharp))

(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

(add-hook 'csharp-mode-hook 'csharp-init-modes)
(add-hook 'csharp-mode-hook 'csharp-init-config)
