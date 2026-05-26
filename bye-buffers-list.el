;;; bye-buffers-list.el --- ...  -*- lexical-binding: t -*-
;;; Commentary:
;;; list of common buffers to hide.
;;; Code:

(setq bye-buffers-list
      '("\\*ag search.*"
        "\\*Async-native-compile-log\\*"
        "\\*Buffer List\\*"
        "\\*compilation\\*"
        "\\*Compile-Log\\*"
        "\\*Completions\\*"
        "\\*company-documentation\\*"
        "\\*EGLOT.*"
        "\\*Flymake.*"
        "\\*Flycheck.*"
        "\\*Help\\*"
        "magit.*"
        "\\*Messages\\*"
        "\\*Packages\\*"
        "\\*Shell Command Output\\*"
        "\\*vc-diff\\*"
        "\\*Warnings\\*"
        "\\*xref\\*"))

;; (setq bye-buffers-show-list
;;       '("\\*Messages\\*"))

;; (defun hide-long-named-buffers (buffer)
;;   (length> (buffer-name buffer) 15))

;; (add-to-list 'bye-buffers-predicates #'hide-long-named-buffers)

(provide 'bye-buffers-list)

;;; bye-buffers-list.el ends here
