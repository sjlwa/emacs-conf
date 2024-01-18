(let (;; temporarily increase `gc-cons-threshold' when loading to speed up startup.
      (gc-cons-threshold most-positive-fixnum)
      ;; Empty to avoid analyzing files when loading remote files.
      (file-name-handler-alist nil))

  ;; Emacs configuration file content is written below.

  (setq default-frame-alist '((width . 100) (height . 40))) ;; Window size



  (setq modus-themes-italic-constructs t)
  (setq modus-themes-common-palette-overrides
	'(
	  (bg-mode-line-active "#003333")
	  (border-mode-line-active "#556666")
	  (bg-region bg-sage)
	  (builtin magenta)
	  (comment "#7A4E54")
	  ;;(constant magenta-cooler)
	  (constant magenta-cooler)
;;	  (docstring green-faint)
	  (docmarkup magenta-faint)
	  (fnname magenta-warmer)
	  (keyword cyan)
	  (preprocessor magenta-warmer)
	  (string blue-cooler)
	  (type magenta-cooler)
	  (variable cyan-faint)
	  (rx-construct magenta-warmer)
	  (rx-backslash blue-cooler)
	  ))
  (load-theme 'modus-vivendi-tritanopia)
  
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
  (electric-pair-mode t)

  ;; melpa
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

  
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
			 "*Help*"
			 (seq "*lsp-" (+ anything))
			 (seq "magit" (+ anything))
			 "*Messages*"
			 "*Packages*"
			 "*pylsp*"
			 "*scratch*"
			 "*Shell Command Output*"
			 (seq "*ts-ls" (+ anything))
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
  ;; Auto save
  (setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-saved-files/" t)))

  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq vc-follow-symlinks t)
  (fset 'yes-or-no-p 'y-or-n-p)
  
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
  

  (use-package company
    :ensure t
    :config
    (setq company-idle-delay 0
	  company-minimum-prefix-length 2
	  company-show-numbers t
	  )    
    (global-company-mode t)
    )

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
    (add-to-list 'company-backends 'company-anaconda)
    )

  (use-package company-quickhelp
  ;; Quickhelp may incorrectly place tooltip towards end of buffer
  ;; See: https://github.com/expez/company-quickhelp/issues/72
  :ensure t
  :config
  (company-quickhelp-mode)
  )


  ;; copy working directory
  (defun sjlwa/cpwd ()
    (interactive)
    (let ((wd (nth 1 (s-split "Directory " (pwd)))))
      (shell-command (concat "echo " wd " | xclip -sel clip &> /dev/null"))
      (prin1 (concat "Copied: " wd))))
   
  

  ;;(global-set-key (kbd "M-s") 'compile)


  ;; programming
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'html-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  (setq css-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.yml?\\'" . yaml-mode))

  ;; rust
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (use-package rust-ts-mode
    :ensure t
    :hook ((rust-ts-mode . lsp-mode)
	   (rust-ts-mode . company-mode)))

 
  ;;
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (use-package go-mode
    :ensure t
    :hook ((go-mode . lsp-mode)
	   (go-mode . company-mode)))





  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize)

  (setq package-selected-packages 
    '(dart-mode lsp-mode lsp-dart lsp-treemacs flycheck company
      ;; Optional packages
      lsp-ui company hover))

  (when (cl-find-if-not #'package-installed-p package-selected-packages)
    (package-refresh-contents)
    (mapc #'package-install package-selected-packages))

  (add-hook 'dart-mode-hook 'lsp)

  (setq gc-cons-threshold (* 100 1024 1024)
	read-process-output-max (* 1024 1024))

  (setq lsp-dart-sdk-dir "/home/sjlwa/snap/flutter/common/flutter/bin/cache/dart-sdk")
  (setq lsp-dart-flutter-sdk "/home/sjlwa/snap/flutter/common/flutter/")
  (setq flutter-sdk-path "/home/sjlwa/snap/flutter/common/flutter/")
  



  
  
  ;;
  (require 'lsp-mode)
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'c-mode-hook #'lsp)

  (global-set-key [backtab] 'emmet-expand-line)
  

  (add-to-list 'load-path "~/.emacs.d/tsx-mode/")
  (setq typescript-indent-level 2) 
  

  (use-package format-all
    :ensure t
    :defer t)
  
  ;; recursive search
  (global-set-key (kbd "C-S-s") 'ag)

  (global-set-key (kbd "C-s") 'occur)

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
   '(lsp-dart dart-mode go-mode pdf-tools edbi company-quickhelp arduino-mode lsp-mode treemacs lsp-treemacs http company dired-sidebar expand-region markdown-mode typescript-mode web-mode yaml-mode)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 136 :width normal :foundry "JB" :family "JetBrains Mono")))))
