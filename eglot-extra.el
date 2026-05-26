;;; eglot-extra --- eglot configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'eglot
  (breadcrumb-mode t)

  (setq eglot-events-buffer-config
        '(:size 1000000 :format full))
  (setq eglot-ignored-server-capabilities
        '(:inlayHintProvider))

  (define-key eglot-mode-map (kbd "C-M-.")
              #'eglot-find-implementation))



;; (setq-default buffer-file-coding-system 'utf-8-unix)

;; (defun my-csharp-unix-line-endings ()
;;   "Ensure C# files are saved with Unix line endings."
;;   (setq buffer-file-coding-system 'utf-8-unix))

;; (add-hook 'csharp-mode-hook 'my-csharp-unix-line-endings)


;; ((csharp-ts-mode . ((eglot-workspace-configuration . (:csharp-ls (:formatting (:endOfLine "\n"))))))
;;  (csharp-mode . ((eglot-workspace-configuration . (:csharp-ls (:formatting (:endOfLine "\n")))))))





;; (add-hook 'eglot 'pixel-scroll-precision-mode)



;; (add-to-list 'eglot-server-programs
;;              `(csharp-mode . ("/usr/lbin/omnisharp" "-lsp")))



;;(load "~/.emacs.d/eglot-booster/eglot-booster.el")
;; (use-package eglot-booster
;; 	:after eglot
;; 	:config	(eglot-booster-mode))

;;(use-package eglot
;;  :custom
;;  (fset #'jsonrpc--log-event #'ignore)
;;  (setf (plist-get eglot-events-buffer-config :size) 0))


(provide 'eglot-extra)

;;; eglot-extra.el ends here
