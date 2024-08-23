;; (add-hook 'html-mode-hook 'emmet-mode)
;; (add-hook 'html-mode-hook 'lsp-deferred)


(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'lsp-deferred)
(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-offset 2)))


(add-hook 'web-mode-hook
          (lambda ()
            (emmet-mode)
            (lsp-deferred)
            (setq web-mode-enable-auto-opening nil
                  web-mode-enable-auto-pairing nil
                  web-mode-enable-auto-closing nil
                  web-mode-enable-auto-quoting nil
                  web-mode-enable-auto-expanding nil
                  web-mode-enable-auto-indentation nil)))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
