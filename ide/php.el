(defun php-init ()
  (web-mode) (lsp))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . php-init))
