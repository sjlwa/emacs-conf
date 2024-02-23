;; Hidden buffers regexp
(setq hidden-buffers-regexp
      (rx (or (and bos " ")
              (and bos
                   (or (seq "*ag search" (+ anything))
					   "*Async-native-compile-log*"
                       "*Buffer List*"
                       "*clang-error*"
                       "*clang-output*"
                       (seq "*clangd" (+ anything))
                       "*compilation*"
                       "*Compile-Log*"
                       "*Completions*"
                       (seq "*EGLOT" (+ anything))
                       (seq "*Flymake" (+ anything))
					   (seq "*gopls" (+ anything))
                       "*Help*"
                       (seq "*lsp-" (+ anything))
                       (seq "magit" (+ anything))
                       "*Messages*"
                       "*Packages*"
                       "*pylsp*"
					   (seq "*iph" (+ anything))
                       ;;"*scratch*"
                       "*Shell Command Output*"
                       (seq "*LSP Dart" (+ anything))
                       (seq "*dart_analysis" (+ anything))
                       (seq "*ts-ls" (+ anything))
                       "*vc-diff*")
                   eos))))

;; Ido Mode
(defun my-ido-mode-config ()
  ;; Vertical mode
  (setq ido-decorations (quote ("\n" "" "\n" "\n ..." "[" "]" "\n    [No match]" " [Matched]" " [Not readable]" " [Too big]" "\n    [Confirm]")))

  (defun ido-disable-line-truncation ()
    (set (make-local-variable 'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

  ;; Hide special buffers
  (setq ido-ignore-buffers (list hidden-buffers-regexp))

  ;; Allow spaces
  (add-hook 'ido-make-file-list-hook
            (lambda ()
              (define-key ido-file-dir-completion-map
                (kbd "SPC") 'self-insert-command)))

  ;; ido up and down
  (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
  (define-key ido-completion-map (kbd "<down>") 'ido-next-match))


(use-package ido
  :defer t
  :config (add-hook 'ido-setup-hook 'my-ido-mode-config)
  :init (ido-mode 1) (setq ido-everywhere t)
  :bind (("C-o" . ido-switch-buffer)
         ("C-p" . my-find-file)))

;; Hide special buffers
(defun my-buffer-skip-p (window buffer bury-or-kill)
  (string-match-p hidden-buffers-regexp (buffer-name buffer)))

(setq switch-to-prev-buffer-skip 'my-buffer-skip-p)

(use-package smex
  :ensure t
  :defer t
  :bind (("M-x" . smex) ("M-X" . smex-major-mode-commands)))

(use-package flx-ido
  :defer t
  :ensure t
  :init (flx-ido-mode 1))
