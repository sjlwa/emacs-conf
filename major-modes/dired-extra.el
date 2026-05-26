;;; Dired --- dired-extra settings. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun dired-load-keymap ()
  "Bind keys for actions on Dired mode."
  (define-key dired-mode-map (kbd "f") 'find-name-dired)
  (define-key dired-mode-map (kbd "<mouse-2>") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "C-<backspace>") 'dired-up-directory)
  (define-key dired-mode-map (kbd "C->") 'dired-show-omited)
  (define-key dired-mode-map (kbd "C-.") 'dired-omit-mode))


(defun dired-configure ()
  "Set default configuration."
  (setq dired-listing-switches "-alh --group-directories-first"
        dired-kill-when-opening-new-dired-buffer t
        dired-omit-files "^\\...+$")
  (put 'dired-find-alternate-file 'disabled nil)
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
  (dired-load-keymap))

(defun dired-show-omited ()
  "Temporarily show only omitted files in Dired."
  (interactive)
  (let ((original-omit-files dired-omit-files))
    ;; Set `dired-omit-files` to show only hidden files (starting with dot)
    (setq dired-omit-files "^[^.]*$")
    (dired-omit-mode -1) ;; Disable to apply new omit settings
    (dired-omit-mode 1)  ;; Enable omit mode with updated settings
    ;; Restore original `dired-omit-files`
    (setq dired-omit-files original-omit-files)))

(defvar dired-text-scale-amount 0
  "Stores the current text scale amount for Dired buffers.")

(defun dired-update-text-scale (&rest args)
  (setq dired-text-scale-amount text-scale-mode-amount))

(defun dired-remain-text-scale-on-readdir ()
  (when (not (eq dired-text-scale-amount 0))
    (text-scale-set dired-text-scale-amount)))

(defun dired-setup ()
  (dired-configure)
  (advice-add 'text-scale-increase :after #'dired-update-text-scale)
  (add-hook 'dired-after-readin-hook #'dired-remain-text-scale-on-readdir))

(with-eval-after-load 'dired (dired-setup))

(provide 'dired-extra)

;;; dired-extra.el ends here
