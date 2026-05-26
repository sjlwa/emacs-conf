;;; -*- lexical-binding: t -*-

(add-to-list 'load-path "~/dev/dotnet-plus")
(require 'dotnet-plus)

(defun eshell/dotnet (&rest params)
  "A custom Eshell function for running 'dotnet' commands as a compilation."
  (interactive)
  (eshell-call-command-as-compilation "dotnet" params))

(defalias 'dotnet-eshell 'eshell/dotnet)

(defun remove-carriage-returns ()
  (interactive)
  (replace-string "\r" ""))

(defun dotnet-format-and-clean ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (eglot-format)
    (remove-carriage-returns)
    (save-buffer)))

(defun dotnet-initialize-modes ()
  (flymake-mode)
  (global-company-mode)
  (electric-indent-mode)

  (setq buffer-file-coding-system 'dos)
  (setq truncate-lines nil
        tab-width 4
        evil-shift-width 4))

(add-to-list 'treesit-language-source-alist
             '(csharp "https://github.com/tree-sitter/tree-sitter-c-sharp"))

(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-ts-mode))
(add-to-list 'auto-mode-alist '("\\.slnx\\'" . csproj-mode))
(add-to-list 'auto-mode-alist '("\\Directory.*.props\\'" . csproj-mode))

(add-hook 'csharp-ts-mode-hook #'dotnet-initialize-modes)

(bye-buffers-hide '("csharp-roslyn" "OmniServer" "omnisharp"))

(with-eval-after-load 'eglot
  (advice-add
   'eglot-code-actions
   :after
   (lambda (&rest _)
     (when (eq major-mode 'csharp-ts-mode)
       (dotnet-format-and-clean)))))


;; (setq-default dotnet-comp-buffer-name "*dotnet-compilation*")

;; (defun dotnet-delete-compilation-frame ()
;;   (interactive)
;;   (if (string-equal (buffer-name) dotnet-comp-buffer-name)
;;       (delete-frame)))

;; (defun dotnet-keymap-load ()
;;   (define-key compilation-mode-map (kbd "q") 'dotnet-delete-compilation-frame)
;;   (define-key compilation-mode-map (kbd "C-w") 'dotnet-delete-compilation-frame))

;; (defun eshell/dotnet (&rest args)
;;   "A custom Eshell function for running 'dotnet' commands as a compilation."
;;   (interactive)
;;   (let ((compilation-buffer (call-command-as-compilation "dotnet" args "*dotnet-compilation*")))
;;     (display-buffer compilation-buffer '(display-buffer-pop-up-frame))))
