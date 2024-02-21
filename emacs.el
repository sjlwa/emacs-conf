;; Temporarily increase `gc-cons-hold' when loading to speed up startup.
(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil ;; Avoid analyzing files when loading remote files.
      inhibit-startup-screen t)

;; default frame
;; uncomment if not loading from .Xresources
;; (setq default-frame-alist '(
;; 							(width . 90) (height . 35) ;; Window size
;; 							(font . "Iosevka-16")
;; 							(tool-bar-lines . 0)
;; 							(menu-bar-lines . 0)
;; 							(vertical-scroll-bars . nil)))

(load-theme 'wald t)

(blink-cursor-mode -1)
(cua-mode 1)
;;(pixel-scroll-precision-mode 1) ;; Doesn't work when lsp-mode is active
(delete-selection-mode +1)
(global-goto-address-mode +1)
(electric-pair-mode t)

(with-eval-after-load 'org
  (setq org-support-shift-select t)
  (setq org-startup-indented t)
  (add-hook 'org-mode-hook #'visual-line-mode))


;; Package Setup
(use-package flx-ido :defer t :ensure t :init (flx-ido-mode 1))
(use-package diff-hl :defer t :ensure t :init (global-diff-hl-mode))
(use-package projectile :ensure t :defer t)
(use-package magit :ensure t :defer t)
(use-package ag :ensure t :defer t)
(use-package http :ensure t :defer t)

;; Hidden buffers regexp
(setq hidden-buffers-regexp
      (rx (or (and bos " ")
              (and bos
                   (or (seq "*ag search" (+ anything))
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

(use-package smex :ensure t :defer t :bind (("M-x" . smex) ("M-X" . smex-major-mode-commands)))

;; Text manipulation
(use-package expand-region :ensure t :defer t :bind (("S-SPC" . er/contract-region) ("C-SPC" . er/expand-region)))

(windmove-default-keybindings 'meta)

(use-package move-text :ensure t :defer t
    :bind (("M-S-<up>" . move-text-up)
	   ("M-S-<down>" . move-text-down)))

(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-saved-files/" t))
      dired-kill-when-opening-new-dired-buffer t
      vc-follow-symlinks t)
(fset 'yes-or-no-p 'y-or-n-p)

;; Tab behaviour
(defun sjlwa/tab ()
  (interactive)
  ;; emmet expansion
  (let ((last-char-before-nil
         (not (or (eq (char-before) 9) ;; tab
                  (eq (char-before) 10) ;; new line
                  (eq (char-before) 32))))) ;; space
    (cond ((and last-char-before-nil (eq major-mode 'web-mode))
           (emmet-expand-line nil)))))

(global-set-key [backtab] 'sjlwa/tab)
(global-set-key "\t" 'indent-rigidly)

(defun backward-delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))
(global-set-key (kbd "C-<backspace>") 'backward-delete-word)


;; Files / buffers
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(global-set-key (kbd "C-w") 'kill-buffer)
(global-set-key (kbd "C-z") 'undo-only)
(global-set-key (kbd "C-S-z") 'undo-redo)

(defun get-def-dir-status-cmd ()
  (format "git -C %s status 2>/dev/null" default-directory))

(defun isrepo ()
  "Verifica si el directorio actual es un repositorio Git."
  (interactive)
  (let ((status (shell-command-to-string (get-def-dir-status-cmd))))
    (> (length status) 0)))

(defun my-find-file ()
  (interactive)
  (if (isrepo)
      (call-interactively 'projectile-find-file)
    (call-interactively 'ido-find-file)))

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

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-show-numbers t)
  (global-company-mode t))

(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  ;;(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  )

(use-package company-anaconda
  :ensure t
  :init (require 'rx)
  :after (company)
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package company-quickhelp :ensure t :config (company-quickhelp-mode))

;; copy working directory
(defun sjlwa/cpwd ()
  (interactive)
  (let ((wd (nth 1 (s-split "Directory " (pwd)))))
    (shell-command (concat "echo " wd " | xclip -sel clip &> /dev/null"))
    (prin1 (concat "Copied: " wd))))



(setq-default tab-width 4)


;; programming

(setq lsp-enable-file-watchers nil)
(setq tooltip-delay 0.1)
(setq eldoc-idle-delay 0)


(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(setq css-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.yml?\\'" . yaml-mode))

;;(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (add-hook 'js2-mode-hook 'skewer-mode)
;; (add-hook 'css-mode-hook 'skewer-css-mode)
;; (add-hook 'html-mode-hook 'skewer-html-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(use-package js2-mode
  :hook ((js2-mode . company-mode)
		 (js2-mode . lsp)))
  

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(use-package rust-ts-mode
  :hook ((rust-ts-mode . lsp)
		 (rust-ts-mode . company-mode)))

(use-package php-mode
  :hook (php-mode . lsp)
  :config (setq php-mode-coding-style 'psr2))

(use-package go-mode
  :hook ((go-mode . company-mode)
		 (go-mode . lsp)))
  ;; :config
  ;; (add-hook 'go-mode-hook #'company-mode)
  ;; (add-hook 'go-mode-hook 'eglot-ensure)
  ;; (add-hook 'after-init-hook #'global-flycheck-mode))

(add-hook 'dart-mode-hook 'lsp)
(setq lsp-dart-sdk-dir "/home/sjlwa/snap/flutter/common/flutter/bin/cache/dart-sdk"
      lsp-dart-flutter-sdk "/home/sjlwa/snap/flutter/common/flutter/"
      flutter-sdk-path "/home/sjlwa/snap/flutter/common/flutter/")

;;(require 'lsp-mode)
;;(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'typescript-mode-hook #'eglot-ensure)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'c-mode-hook #'lsp)

(add-hook 'prog-mode-hook 'hs-minor-mode)
(global-set-key (kbd "C-c SPC") 'hs-toggle-hiding)

(add-to-list 'load-path "~/.emacs.d/tsx-mode/")
(setq typescript-indent-level 2)

(use-package format-all :ensure t :defer t)

;; recursive search
(global-set-key (kbd "C-S-s") 'ag)

;; (global-set-key (kbd "C-s") 'occur)

;; Scale text
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; (setq org-agenda-directory '("~/OrgAgenda"))
;; (global-set-key (kbd "C-c a") 'org-agenda)

(global-set-key (kbd "<s-left>") 'beginning-of-line)
(global-set-key (kbd "<s-right>") 'end-of-line)

(put 'dired-find-alternate-file 'disabled nil)

;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; ;; (use-package esup
;; ;;   :ensure t
;; ;;   ;; To use MELPA Stable use ":pin melpa-stable",
;; ;;   :pin melpa)
(setq esup-depth 0)
;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(standard-themes eldoc-overlay eldoc-box php-mode yaml-mode websocket web-server web-mode typescript-mode smex skewer-mode projectile pdf-tools paredit origami move-text magit lsp-ui lsp-dart list-utils js2-refactor iedit http hover helm go-mode format-all flycheck flx-ido expand-region esup emmet-mode edbi dumb-jump dired-sidebar diff-hl coverlay company-web company-quickhelp company-anaconda clang-format arduino-mode ag)))
