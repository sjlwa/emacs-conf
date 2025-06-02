(defun eshell/dotnet (&rest args)
  "A custom Eshell function for running 'dotnet' commands as a compilation."
  (interactive)
  (eshell-call-command-as-compilation "dotnet" args))

(defun dap-netcore-enable ()
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
  (lsp))

(defun csharp-init-modes ()
  (flycheck-mode)
  (lsp-csharp-roslyn-enable)
  (dap-netcore-enable))

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
  (add-hook 'csharp-mode-hook 'csharp-set-config)
  (add-hook 'csharp-mode-hook 'csharp-init-modes)

  (bye-buffers-add-patterns-inbetween hidden-buffers
                                      '("csharp-roslyn" "OmniServer" "omnisharp"))

  (msg "Mode config loaded: chsarp-mode"))
