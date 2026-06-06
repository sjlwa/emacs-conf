;;; org-extra.el --- org mode configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(let ((gc-cons-threshold most-positive-fixnum))
  (require 'org))

(defun org-mode-boost-gc (orig-fun &rest args)
  (let ((gc-cons-threshold most-positive-fixnum))
    (apply orig-fun args)))

(advice-add 'org-mode :around #'org-mode-boost-gc)

(defun org-mode-define-tasks-status ()
  (setq org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE")))
  (setq org-todo-keyword-faces
      '(("TODO" . org-todo)
        ("DOING" . org-drawer)
        ("DONE" . org-done))))

(defun org-mode-define-config ()
    (setq org-startup-folded nil)            ;; Show all content, no folding
    (setq org-startup-with-inline-images nil) ;; Don't show inline images
    (setq org-hide-emphasis-markers nil)     ;; Show emphasis markers

    (setq org-support-shift-select t
          org-startup-indented t)
    (org-mode-define-tasks-status)
    (add-hook 'org-mode-hook #'visual-line-mode))

(with-eval-after-load 'org (org-mode-define-config))

(provide 'org-extra)

;;; org-extra.el ends here
