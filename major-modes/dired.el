(defun dired-load-keymap ()
  "Bind keys for actions on dired mode."
  (define-key dired-mode-map (kbd "<mouse-2>") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "C-<backspace>") 'dired-up-directory)
  (define-key dired-mode-map (kbd "C->") 'dired-show-omited)
  (define-key dired-mode-map (kbd "C-.") 'dired-omit-mode))

(defun dired-configure ()
  (setq dired-kill-when-opening-new-dired-buffer t)
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-omit-files "^\\...+$")
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

  (with-eval-after-load 'dired (dired-load-keymap)))

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


;; (defvar my-dired-text-scale 0)

;; (defun my/save-dired-zoom ()
;;   "Guardar la escala de texto antes de abrir un nuevo buffer Dired."
;;   (when (eq major-mode 'dired-mode)
;;     (setq my-dired-text-scale text-scale-mode-amount)))

;; (defun my/restore-dired-zoom ()
;;   "Restaurar la escala de texto después de abrir un buffer Dired."
;;   (when (eq major-mode 'dired-mode)
;;     (text-scale-set my-dired-text-scale)))

;; (add-hook 'dired-mode-hook 'my/save-dired-zoom)
;; (add-hook 'dired-mode-hook 'my/restore-dired-zoom)
