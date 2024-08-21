;; Temporarily increase `gc-cons-hold' when loading to speed up startup.
(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil ;; Avoid analyzing files when loading remote files.
      inhibit-startup-screen t
	  warning-minimum-level :emergency)


(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

;; default frame
;; uncomment if not loading from .Xresources
;; (setq default-frame-alist '(
;; 							(width . 90) (height . 35) ;; Window size
;; 							(font . "Iosevka-16")
;; 							(tool-bar-lines . 0)
;; 							(menu-bar-lines . 0)
;; 							(vertical-scroll-bars . nil)))
;; (blink-cursor-mode -1)


;;(pixel-scroll-precision-mode 1) ;; Doesn't work when lsp-mode is active
(cua-mode 1)

(if (daemonp)
    (progn
      (load-theme 'boron)
      (fringe-mode 0))
  
  (if window-system
      (load-theme 'modus-vivendi)
    (xterm-mouse-mode 1))
  )


(column-number-mode 1)
(delete-selection-mode +1)
(global-goto-address-mode +1)
(electric-pair-mode t)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(put 'dired-find-alternate-file 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings 'meta)
(setq-default tab-width 4)


(with-eval-after-load 'org
  (setq org-support-shift-select t
        org-startup-indented t)
  (set-org-mode-tasks-status)
  (add-hook 'org-mode-hook #'visual-line-mode))


(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-saved-files/" t))
      dired-kill-when-opening-new-dired-buffer t
      vc-follow-symlinks t
      tooltip-delay 0.1
      eldoc-idle-delay 0
      indent-tabs-mode nil)


;;(setq lsp-enable-on-type-formatting nil)
(setq-default indent-tabs-mode nil)
(setq-default electric-indent-inhibit t)


(load "~/dev/emacs-conf/ide.el")

(load-file "~/dev/emacs-conf/ide/web.el")
;;(load-file "~/dev/emacs-conf/ide/javascript.el")
;;(load-file "~/dev/emacs-conf/ide/rust.el")
;;(load-file "~/dev/emacs-conf/ide/c.el")
;;(load-file "~/dev/emacs-conf/ide/python.el")
;;(load-file "~/dev/emacs-conf/ide/php.el")
;;(load-file "~/dev/emacs-conf/ide/golang.el")
(load-file "~/dev/emacs-conf/ide/typescript.el")
;;(load-file "~/dev/emacs-conf/ide/java.el")
;;(load-file "~/dev/emacs-conf/ide/dart.el")
(load-file "~/dev/emacs-conf/ide/csharp.el")

 (setq treesit-language-source-alist
       '(
		 (bash "https://github.com/tree-sitter/tree-sitter-bash")
		 (cmake "https://github.com/uyha/tree-sitter-cmake")
		 (c "https://github.com/tree-sitter/tree-sitter-c")
         (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp.git")
		 (css "https://github.com/tree-sitter/tree-sitter-css")
		 (elisp "https://github.com/Wilfred/tree-sitter-elisp")
		 (go "https://github.com/tree-sitter/tree-sitter-go")
		 (gomod "https://github.com/camdencheek/tree-sitter-go-mod.git")
		 (html "https://github.com/tree-sitter/tree-sitter-html")
         (java "https://github.com/tree-sitter/tree-sitter-java")
		 (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
		 (json "https://github.com/tree-sitter/tree-sitter-json")
		 (make "https://github.com/alemuller/tree-sitter-make")
		 (markdown "https://github.com/ikatyang/tree-sitter-markdown")
		 (python "https://github.com/tree-sitter/tree-sitter-python")
		 (php "https://github.com/tree-sitter/tree-sitter-php.git")
		 (toml "https://github.com/tree-sitter/tree-sitter-toml")
		 (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
		 (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
         (rust "https://github.com/tree-sitter/tree-sitter-rust.git")
		 (yaml "https://github.com/ikatyang/tree-sitter-yaml")))


(add-hook 'after-init-hook #'(lambda ()
							   (load "~/dev/emacs-conf/fuzzy.el")
							   (load "~/dev/emacs-conf/bindings.el")
							   (load "~/dev/emacs-conf/commands.el")
							   (load "~/dev/emacs-conf/pkgconf.el")
							   ))

;;(setq eshell-prompt-function 'sjlwa-eshell-prompt)
;;(setq eshell-highlight-prompt nil)
(use-package esh-autosuggest :ensure t
  :config (setq esh-autosuggest-use-company-map t))
(add-hook 'eshell-mode-hook 'eshell-init-setup)
(add-hook 'after-change-major-mode-hook 'sjlwa/toggle-mode-line-based-on-mode)


;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(esh-autosuggest editorconfig csproj-mode verb omnisharp mermaid-mode lsp-java web-mode ellama rust-mode evil rainbow-mode multiple-cursors php-mode vmd-mode php-quickhelp dockerfile-mode lsp-pyright company-jedi elpy go-mode tree-sitter-langs tree-sitter standard-themes eldoc-overlay eldoc-box yaml-mode websocket web-server smex skewer-mode projectile pdf-tools paredit origami move-text magit lsp-ui lsp-dart list-utils js2-refactor iedit http hover helm format-all flycheck expand-region esup emmet-mode dumb-jump dired-sidebar diff-hl coverlay company-web company-quickhelp clang-format arduino-mode ag)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
