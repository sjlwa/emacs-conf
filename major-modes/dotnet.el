;;; -*- lexical-binding: t -*-

(defun eshell/dotnet (&rest args)
  "A custom Eshell function for running 'dotnet' commands as a compilation."
  (interactive)
  (eshell-call-command-as-compilation "dotnet" args))

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

(defalias 'dotnet-eshell 'eshell/dotnet)

;; (add-to-list 'display-buffer-alist
;;              `(,(rx bos ,dotnet-comp-buffer-name eos)
;;                (display-buffer-reuse-window display-buffer-pop-up-frame)
;;                (reusable-frames . visible)))


(defun dotnet-dap-netcore-enable ()
  (require 'dap-netcore)
  (require 'dap-utils)
  (setq dap-print-io t)
  (setq dap-netcore-install-dir "~/.emacs.d/.cache/")
  (setq dap-netcore-download-url
        "https://github.com/Samsung/netcoredbg/releases/download/3.1.2-1054/netcoredbg-linux-amd64.tar.gz")
  (dap-auto-configure-mode))

(defun lsp-csharp-roslyn-enable ()
  (require 'lsp-mode)
  (setq lsp-disabled-clients '(csharp-ls omnisharp))
  (setq lsp-client-packages (remove 'lsp-csharp lsp-client-packages))
  (setq lsp-csharp-server-args
        (list "--languageserver" "--hostPID" (format "%d" (emacs-pid))))
  (lsp)
  )

;; (defun custom-lsp-roslyn--on-initialized (workspace)
;;   (lsp-roslyn-open-solution-file)
;;   (with-lsp-workspace workspace
;;     (lsp--set-configuration
;;      #s(hash-table
;;          size 30
;;          test equal
;;          data (
;;                "csharp|background_analysis.dotnet_analyzer_diagnostics_scope" "fullSolution"
;;                "csharp|background_analysis.dotnet_compiler_diagnostics_scope" "fullSolution"
;;                )))))

;; (advice-add 'lsp-roslyn--on-initialized :override #'custom-lsp-roslyn--on-initialized)


;; (defun roslyn-get-executable-list ()
;;   '("dotnet"
;;     "exec"
;;     "/home/sjlwa/.emacs.d/.cache/lsp/roslyn/out/microsoft.codeanalysis.languageserver.linux-x64/4.13.0-2.24564.12/content/LanguageServer/linux-x64/Microsoft.CodeAnalysis.LanguageServer.dll"
;;     "--logLevel" "Information"
;;     "--extensionLogDirectory" "/home/sjlwa/.roslyn-lsp-logs"))

;; (defun eglot-csharp-roslyn-enable ()
;;   (with-eval-after-load 'eglot
;;     (add-to-list 'eglot-server-programs
;;                  `(csharp-mode . ,(roslyn-get-executable-list)))))


(defun csharp-init-modes ()
  ;; (flycheck-mode)
  (lsp-csharp-roslyn-enable)
  (dotnet-dap-netcore-enable))

(defun csharp-set-config ()
  (c-set-style "ellemtel")
  (setq indent-tabs-mode nil
        c-syntactic-indentation t
        c-basic-offset 4
        truncate-lines t
        tab-width 4
        evil-shift-width 4))

(defun load-lang--csharp ()
  "Loads the required configuration for csharp mode"
  (add-to-list 'treesit-language-source-alist
               '(csharp "https://github.com/tree-sitter/tree-sitter-c-sharp"))

  (add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
  (add-hook 'csharp-mode-hook 'csharp-set-config)
  (add-hook 'csharp-mode-hook 'csharp-init-modes)

  (bye-buffers-add-inbetween '("csharp-roslyn" "OmniServer" "omnisharp"))

  (msg "Mode config loaded: chsarp-mode"))

(load-lang--csharp)
