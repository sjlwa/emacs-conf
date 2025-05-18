;;; -*- lexical-binding: t -*-

(defun company-init-config ()
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-show-numbers t
        company-tooltip-width-grow-only t
        company-tooltip-maximum-width 40
        company-tooltip-minimum-width 40
        company-tooltip-offset-display 'lines
        company-tooltip-align-annotations nil)

  (unless (eq major-mode 'eshell-mode)
    (setq company-transformers '(;; delete-consecutive-dups ;; TODO: Fix crash eshell with esh-autocomplete.
                                 company-sort-by-occurrence)))

  (if (require 'company-quickhelp nil 'noerror)
      (company-quickhelp-mode)))

(defun company-keymap-load ()
  (define-key company-active-map (kbd "M-h") #'company-show-doc-buffer)
  (define-key company-active-map (kbd "C-h") #'ryanmarcus/backward-kill-word))

(with-eval-after-load 'company
  (company-keymap-load)
  (add-hook 'company-mode-hook 'company-init-config))
