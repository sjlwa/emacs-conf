;;; package --- icomplete-extra -*- lexical-binding: t -*-
;;; Code:

(require 'cl-lib)

(defun icomplete-force-complete-custom ()
  "Force completion of the current icomplete candidate."
  (interactive)
  (when (bound-and-true-p icomplete-mode)
    (let ((icomplete-prospects-height 1))
      (icomplete-force-complete))))

  (defun icomplete-file-sort (candidates)
  "Sort candidates by directory depth (fewer slashes first), then alphabetically."
  (sort candidates
         (lambda (a b)
           (let ((da (cl-count ?/ a))
                 (db (cl-count ?/ b)))
             (if (/= da db)
                 (< da db) ; Shallowest paths first
               (string-version-lessp a b))))))

(with-eval-after-load 'icomplete
  ;; (savehist-mode 1)
  (setq completion-ignore-case t
        completion-styles '(flex basic)
        completions-sort 'alphabetical)
  ;; completions-sort 'historical)

    (setq completion-category-overrides
         '((file (styles basic partial-completion flex)
                 (cycle-sort-function . icomplete-file-sort))
           (project-file (styles basic partial-completion flex)
                         (cycle-sort-function . icomplete-file-sort))))

  (define-key icomplete-minibuffer-map (kbd "TAB") #'icomplete-force-complete-custom))

(fido-vertical-mode)







;; (defvar minibuffer-origin-command nil)

;; (defun minibuffer-save--origin-command ()
;;   (setq minibuffer-origin-command this-command))

;; (add-hook 'minibuffer-setup-hook #'minibuffer-save--origin-command)

;; (defun icomplete-get-current-match ()
;;   (when (minibufferp)
;;     (let* ((beg (icomplete--field-beg))
;;            (end (icomplete--field-end))
;;            (comps (completion-all-sorted-completions beg end)))
;;       (substring-no-properties (car comps)))))




;; (defvar icomplete-preview--last-path nil
;;   "Ruta del último archivo mostrado en el preview.")



;; (defconst icomplete-preview--buffer-name "*file-preview*"
;;   "Nombre fijo del buffer de preview.")

;; (defun icomplete-preview-file (path)
;;   "Muestra una vista previa de PATH en un buffer reutilizable.
;; Detecta el modo mayor por extensión, sin ejecutar hooks."
;;   (if (not (and path (file-exists-p path)))
;;       (icomplete-preview--cleanup)
;;     (unless (equal path icomplete-preview--last-path)
;;       (setq icomplete-preview--last-path path)
;;       (let ((buf (get-buffer-create icomplete-preview--buffer-name)))
;;         (with-current-buffer buf
;;           (let ((inhibit-major-mode-hooks t) ;; ← custom flag que usamos abajo
;;                 (buffer-read-only nil))
;;             (erase-buffer)
;;             (cond
;;              ((file-directory-p path)
;;               (insert (format "Directory: %s\n\n" path))
;;               (dolist (f (directory-files path t directory-files-no-dot-files-regexp))
;;                 (insert (concat (file-name-nondirectory f)
;;                                 (if (file-directory-p f) "/" "")
;;                                 "\n"))))
;;              (t
;;               (insert-file-contents path nil 0 2000)
;;               (let ((auto-mode-case-fold t)
;;                     (inhibit-local-variables t)) ;; evita ejecución de .dir-locals.el
;;                 ;; Detectar modo mayor por extensión, sin hooks
;;                 (let ((mode (assoc-default path auto-mode-alist 'string-match)))
;;                   (when (and mode (functionp mode))
;;                     (let ((inhibit-local-variables t)) ;; seguridad extra
;;                       (funcall mode))
;;                     ;; Borrar cualquier hook residual
;;                     (setq-local after-change-functions nil)
;;                     (setq-local post-command-hook nil)
;;                     (setq-local before-save-hook nil))))))
;;             (goto-char (point-min))
;;             (view-mode 1)))
;;         (let ((display-buffer-alist
;;                `((,icomplete-preview--buffer-name
;;                   (display-buffer-reuse-window display-buffer-at-bottom)
;;                   (window-height . 15)))))
;;           (display-buffer buf))))))




;; (defun icomplete-preview--cleanup ()
;;   "Cierra el buffer de preview y resetea el último path."
;;   (setq icomplete-preview--last-path nil)
;;   (when-let ((buf (get-buffer "*file-preview*")))
;;     (kill-buffer buf)))

;; (defun icomplete-preview-current-file (&rest _args)
;;   "Muestra una vista previa del archivo seleccionado actualmente en fido/icomplete."
;;   (when (and (eq minibuffer-origin-command 'find-file)
;;              (minibufferp))
;;     (let ((match (icomplete-get-current-match)))
;;       (when (and match (not (string-empty-p match)))
;;         (let ((path (expand-file-name match default-directory)))
;;           (icomplete-preview-file path))))))

;; ;; Hook para cerrar preview al salir del minibuffer
;; (add-hook 'minibuffer-exit-hook #'icomplete-preview--cleanup)

;; ;; Advice para actualizar preview al moverse entre ítems
;; (advice-add 'icomplete-forward-completions :after #'icomplete-preview-current-file)
;; (advice-add 'icomplete-backward-completions :after #'icomplete-preview-current-file)


(provide 'icomplete-extra)

;;; icomplete-extra.el ends here
