(let (;; temporarily increase `gc-cons-threshold' when loading to speed up startup.
      (gc-cons-threshold most-positive-fixnum)
      ;; Empty to avoid analyzing files when loading remote files.
      (file-name-handler-alist nil))

  ;; Emacs configuration file content is written below.

  (setq default-frame-alist '((width . 100) (height . 40))) ;; Window size
  (load-theme 'modus-vivendi-deuteranopia)
  (setq cursor-type 'bar)
  (setq inhibit-startup-screen t)

  ;; Visual Modes
  (blink-cursor-mode -1)
  (column-number-mode +1)
  (fringe-mode 4)
  (tool-bar-mode -1) 
  (scroll-bar-mode -1)
  
  ;;
  (cua-mode 1)
  (pixel-scroll-precision-mode 1)
  (recentf-mode 1)
  (delete-selection-mode +1)
  (global-goto-address-mode +1)
  (global-visual-line-mode +1)
  (move-text-default-bindings)

  ;; ;; MELPA
  ;; (require 'package)
  ;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  ;; (package-initialize)

  
  ;; ;; Multiple Cursors
  ;; (require 'multiple-cursors)
  ;; (global-set-key (kbd "C-d") 'mc/edit-lines)
  ;; (global-set-key (kbd "C-.") 'mc/mark-next-like-this)
  ;; (global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
  ;; (global-set-key (kbd "C->") 'mc/mark-all-like-this)

  (use-package flx-ido
    :ensure t
    :defer t
    :init (flx-ido-mode 1))

  (use-package diff-hl
    :ensure t
    :defer t
    :init (global-diff-hl-mode))
  
  (use-package projectile :ensure t :defer t)

  (use-package magit :ensure t :defer t)

  (use-package ag :ensure t :defer t)  

  ;; Hiden special buffers regexp
  (setq hidden-buffers-regexp
	(rx (or (and bos  " ")
		(and bos
		     (or "*Buffer List*"
			 "*clang-output*"
			 "*compilation*"
			 "*Compile-Log*"
			 "*Completions*"
			 (seq "*EGLOT" (+ anything))
			 "*Help*"
			 (seq "magit" (+ anything))
			 "*Messages*"
			 "*Packages*"
			 "*scratch*"
			 "*Shell Command Output*"
			 "*vc-diff*")
		     eos))))
   
  ;; Ido Mode
  (defun my-ido-mode-config ()

      ;; Vertical mode
    (setq ido-decorations (quote ("\n" "" "\n" "\n ..." "[" "]" "
    [No match]" " [Matched]" " [Not readable]" " [Too big]" "
    [Confirm]")))

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
    (define-key ido-completion-map (kbd "<up>")   'ido-prev-match)
    (define-key ido-completion-map (kbd "<down>") 'ido-next-match))



  ;; Commands
  (use-package smex
    :defer t
    :bind (("M-x" . smex)
	   ("M-X" . smex-major-mode-commands)))


  ;; Text manipulation
  (use-package expand-region
    :defer t
    :bind (("S-SPC" . er/contract-region)
	   ("C-SPC" . er/expand-region)))
    
  (windmove-default-keybindings 'meta)
  (global-set-key (kbd "TAB") 'indent-rigidly)

  (use-package move-text
    :ensure t
    :defer t
    :bind (("M-S-<up>" . move-text-up)
	   ("M-S-<down>" . move-text-down)))

  ;; Backups
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

  ;; Files / buffers
  (global-set-key "\C-x\ \C-r" 'recentf-open-files)
  (global-set-key (kbd "C-w") 'kill-buffer)
  

  (defun get-def-dir-status-cmd ()
    (format "git -C %s status 2>/dev/null" default-directory))

  (defun isrepo ()
    "Verifica si el directorio actual es un repositorio Git."
    (interactive)
    (let ((status (shell-command-to-string (get-def-dir-status-cmd))))
      (if (> (length status) 0)
	  t
	nil)))

  (defun my-find-file ()
    (interactive)
    (if (isrepo)
	(call-interactively 'projectile-find-file)
      (call-interactively 'ido-find-file)))


  (use-package ido
    :defer t
    :config (add-hook 'ido-setup-hook 'my-ido-mode-config)
    :init
    (ido-mode 1)
    (setq ido-everywhere t)

    
    :bind (("C-o" . ido-switch-buffer)
	   ("C-p" . my-find-file)))

  
  ;; Hide special buffers
  (defun my-buffer-skip-p (window buffer bury-or-kill)
    (string-match-p hidden-buffers-regexp (buffer-name buffer)))

  (setq switch-to-prev-buffer-skip 'my-buffer-skip-p)

  

  (defun load-company-hook ()
    ;; (defun tab-indent-or-complete ()
    ;;   (interactive)
    ;;   (if (minibufferp)
    ;;       (minibuffer-complete)
    ;;     (if (or (not yas-minor-mode)
    ;;             (null (do-yas-expand)))
    ;;         (if (check-expansion)
    ;;             (company-complete-common)
    ;;           (indent-for-tab-command)))))

    ;; (global-set-key [backtab] 'tab-indent-or-complete)
    
    ;;(setq company-dabbrev-downcase 0)
    ;;(setq company-idle-delay 0)
    (company-mode 1)
    (global-company-mode)
    (setq company-minimum-prefix-length 1))

  
  
  ;; programming
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'html-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  (setq css-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.yml?\\'" . yaml-mode))

  (global-set-key [backtab] 'emmet-expand-line)
  
  (add-hook 'c-mode-hook 'load-company-hook)
  (add-hook 'python-mode-hook 'load-company-hook)
  (add-hook 'emacs-lisp-mode-hook 'load-company-hook)
 

  ;; (setq treesit-language-source-alist
  ;; 	'((tsx
  ;; 	   "https://github.com/tree-sitter/tree-sitter-typescript"
  ;; 	   "master"
  ;; 	   "tsx/src")
	  
  ;; 	  (typescript
  ;; 	   "https://github.com/tree-sitter/tree-sitter-typescript"
  ;; 	   "master"
  ;; 	   "typescript/src")))


  (use-package format-all
    :ensure t
    :defer t)
  
  ;; recursive search
  (global-set-key (kbd "C-S-s") 'ag)

  ;; Scale text
  (global-set-key (kbd "C-+") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)

  
  ) ;;



(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(package-selected-packages
    '(
      company
      dired-sidebar
      expand-region
      markdown-mode
      ;;multiple-cursors
      typescript-mode
      web-mode
      yaml-mode
      )))


(custom-set-faces
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
'(default ((t (:inherit nil
	       :extend nil
	       :stipple nil
	       :inverse-video nil
	       :box nil
	       :strike-through nil
	       :overline nil
	       :underline nil
	       :slant normal
	       :weight normal
	       :height 136
	       :width normal
	       :foundry "JB"
	       :family "JetBrains Mono")))))
