(let (;; temporarily increase `gc-cons-threshold' when loading to speed up startup.
      (gc-cons-threshold most-positive-fixnum)
      ;; Empty to avoid analyzing files when loading remote files.
      (file-name-handler-alist nil))

  ;; Emacs configuration file content is written below.

  ;; MELPA
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

  ;; Window size
  (setq default-frame-alist '((width . 100) (height . 40)))

  (custom-set-variables
    ;; custom-set-variables was added by Custom.
    ;; If you edit it by hand, you could mess it up, so be careful.
    ;; Your init file should contain only one such instance.
    ;; If there is more than one, they won't work right.
    '(css-indent-offset 2)
    '(cua-mode 1)
    '(cursor-type 'bar)
    '(custom-enabled-themes '(modus-vivendi-deuteranopia))
    '(custom-safe-themes
      '("74e2ed63173b47d6dc9a82a9a8a6a9048d89760df18bc7033c5f91ff4d083e37" default))
    '(flx-ido-mode 1)
    '(ido-mode 1 nil (ido))
    '(inhibit-startup-screen t)
    '(package-selected-packages
      '(ag
	company
	diff-hl
	dired-sidebar
	expand-region
	flx-ido projectile
	format-all
	kaolin-themes
	magit
	markdown-mode
	move-text
	multiple-cursors
	neotree
	smex
	typescript-mode
	vs-dark-theme
	web-mode
	yaml-mode
	))
    '(projectile-require-project-root nil))


  
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


  
  ;; Multiple Cursors
  (require 'multiple-cursors)
  (global-set-key (kbd "C-d") 'mc/edit-lines)
  (global-set-key (kbd "C-.") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C->") 'mc/mark-all-like-this)

  ;; Modes
  (blink-cursor-mode -1)
  (column-number-mode +1)
  (fringe-mode 4)
  (tool-bar-mode -1) 
  (scroll-bar-mode -1)
  (global-goto-address-mode +1)
  (global-visual-line-mode +1)
  (delete-selection-mode +1)
  (recentf-mode 1)
  (global-diff-hl-mode)
  (pixel-scroll-precision-mode 1)

  (move-text-default-bindings )

  ;; Ido Mode
  (defun my-ido-mode-config ()
    (setq ido-everywhere t)

      ;; Vertical mode
    (setq ido-decorations (quote ("\n" "" "\n" "\n ..." "[" "]" "
    [No match]" " [Matched]" " [Not readable]" " [Too big]" "
    [Confirm]")))

    (defun ido-disable-line-truncation ()
      (set (make-local-variable 'truncate-lines) nil))
    (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

    ;; Hide special buffers
    (setq ido-ignore-buffers (list (rx (or (and bos  " ")
	(and bos
	    (or "*Completions*"
		"*Shell Command Output*"
		"*Messages*"
		"*compilation*"
		"*clang-output*"
		"*vc-diff*")
	    eos)))))

    ;; Allow spaces
    (add-hook 'ido-make-file-list-hook
      (lambda ()
	(define-key ido-file-dir-completion-map
	  (kbd "SPC") 'self-insert-command)))

    ;; ido up and down
    (define-key ido-completion-map (kbd "<up>")   'ido-prev-match)
    (define-key ido-completion-map (kbd "<down>") 'ido-next-match))

  (add-hook 'ido-setup-hook 'my-ido-mode-config)

  
  ;; Commands
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;; Old M-x.

  ;; Text manipulation
  (windmove-default-keybindings 'meta)
  (global-set-key (kbd "C-SPC") 'er/expand-region)
  (global-set-key (kbd "C-S-SPC") 'er/contract-region)
  (global-set-key (kbd "TAB") 'indent-rigidly)
  (global-set-key (kbd "M-S-<up>") 'move-text-up)
  (global-set-key (kbd "M-S-<down>") 'move-text-down)

  ;; Backups
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))


  
  ;; Files / buffers
  (global-set-key "\C-x\ \C-r" 'recentf-open-files)
  (global-set-key (kbd "C-w") 'kill-buffer)
  (global-set-key (kbd "C-o") 'ido-switch-buffer)
  (global-set-key (kbd "C-b") 'ivy-switch-buffer)

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

  (global-set-key (kbd "C-p") 'my-find-file)



  
  ;; programming
  (emmet-mode 1)
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'html-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.yml?\\'" . yaml-mode))

  (global-set-key [backtab] 'emmet-expand-line)

  (company-mode 1)
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-minimum-prefix-length 1)
  ;;(setq company-dabbrev-downcase 0)
  ;;(setq company-idle-delay 0)

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


  ;; (setq treesit-language-source-alist
  ;; 	'((tsx
  ;; 	   "https://github.com/tree-sitter/tree-sitter-typescript"
  ;; 	   "master"
  ;; 	   "tsx/src")
	  
  ;; 	  (typescript
  ;; 	   "https://github.com/tree-sitter/tree-sitter-typescript"
  ;; 	   "master"
  ;; 	   "typescript/src")))

  
  
  ;; recursive search
  (global-set-key (kbd "C-S-s") 'ag)

  ;; Scale text
  (global-set-key (kbd "C-+") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)

  
  ) ;; END OF STARTUP

