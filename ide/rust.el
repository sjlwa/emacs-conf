(use-package rust-mode
  :hook ((rust-mode . lsp)
		 (rust-mode . company-mode)))

;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
;; (use-package rust-ts-mode
;;   :hook ((rust-ts-mode . lsp)
;; 		 (rust-ts-mode . company-mode)))
