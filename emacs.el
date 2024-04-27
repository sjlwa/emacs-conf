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


;;(require 'package)
;;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(load-file "~/.emacs.d/ellama/ellama.el")
(use-package ellama
  :init
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
		  (make-llm-ollama
		   :chat-model "llama3" :embedding-model "llama3")))

;;(pixel-scroll-precision-mode 1) ;; Doesn't work when lsp-mode is active
(cua-mode 1)
(load-theme 'amena-olive)
(delete-selection-mode +1)
(global-goto-address-mode +1)
(electric-pair-mode t)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(put 'dired-find-alternate-file 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings 'meta)
(setq-default tab-width 4)
(setq tooltip-delay 0.1)
(setq eldoc-idle-delay 0)


(with-eval-after-load 'org
  (setq org-support-shift-select t)
  (setq org-startup-indented t)
  (add-hook 'org-mode-hook #'visual-line-mode))


(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-saved-files/" t))
      dired-kill-when-opening-new-dired-buffer t
      vc-follow-symlinks t)


(load-file "~/.emacs.d/codeium.el/codeium.el")
(use-package codeium
  :init
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  :config
    (setq use-dialog-box nil)
  )


(use-package company :ensure t
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-show-numbers t
        company-tooltip-offset-display 'lines)
  (global-company-mode t))

(use-package company-quickhelp  :ensure t :config (company-quickhelp-mode))

;; (use-package tree-sitter :ensure t)
;; (use-package tree-sitter-langs :ensure t)

(use-package lsp-mode :ensure t :config (setq lsp-enable-file-watchers nil))

;;(setq lsp-enable-on-type-formatting nil)
(setq-default indent-tabs-mode nil)
(setq-default electric-indent-inhibit t)
(setq indent-tabs-mode nil)
(setq web-mode-enable-auto-opening nil)
(setq web-mode-enable-auto-pairing nil)
(setq web-mode-enable-auto-closing nil)
(setq web-mode-enable-auto-quoting nil)
(setq web-mode-enable-auto-expanding nil)
(setq web-mode-enable-auto-indentation nil)
;;(setq web-mode-code-indent-offset 2)

;; (use-package emmet-mode :ensure t)

(use-package html-mode :hook ((css-mode . emmet-mode) (css-mode . lsp)))

(use-package css-mode :hook ((css-mode . emmet-mode) (css-mode . lsp)) :config (setq css-indent-offset 2))

(add-to-list 'auto-mode-alist '("\\.yml?\\'" . yaml-mode))

;;(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (add-hook 'js2-mode-hook 'skewer-mode)
;; (add-hook 'css-mode-hook 'skewer-css-mode)
;; (add-hook 'html-mode-hook 'skewer-html-mode)
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (use-package js2-mode
;;   :hook ((js2-mode . company-mode)
;; 		 (js2-mode . lsp)))
  

;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
;; (use-package rust-ts-mode
;;   :hook ((rust-ts-mode . lsp)
;; 		 (rust-ts-mode . company-mode)))

(use-package rust-mode
  :hook ((rust-mode . eglot-ensure)
		 (rust-mode . company-mode)))




;;(load "~/.emacs.d/eglot-booster/eglot-booster.el")
;; (use-package eglot-booster
;; 	:after eglot
;; 	:config	(eglot-booster-mode))

;;(use-package eglot
;;  :custom
;;  (fset #'jsonrpc--log-event #'ignore)
;;  (setf (plist-get eglot-events-buffer-config :size) 0))

(use-package c-ts-mode :mode (("\\.c\\'" . c-ts-mode)) :hook (c-ts-mode . eglot-ensure))

;;(use-package lsp-pyright :ensure t)
(use-package python-ts-mode
  :mode (("\\.py\\'" . python-ts-mode))
  :hook ((python-ts-mode . lsp)
		 (python-ts-mode . company-mode))
  :config (setq lsp-pyright-typechecking-mode "strict"))


(defun php-init () (web-mode) (lsp))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . php-init))


(use-package go-ts-mode
  :mode (("\\.go\\'" . go-ts-mode)
		 ("\\.mod\\'" . go-mod-ts-mode))
  :hook ((go-ts-mode . company-mode)
		(go-ts-mode . eglot-ensure)
		(go-ts-mode . global-flycheck-mode)))


(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode))
  :hook (typescript-ts-mode . eglot-ensure))

;;(add-to-list 'load-path "~/.emacs.d/tsx-mode/")
(setq typescript-indent-level 2)
;;(add-hook 'typescript-mode-hook #'eglot-ensure)


(add-hook 'dart-mode-hook 'lsp)
(setq lsp-dart-sdk-dir "/home/sjlwa/snap/flutter/common/flutter/bin/cache/dart-sdk"
      lsp-dart-flutter-sdk "/home/sjlwa/snap/flutter/common/flutter/"
      flutter-sdk-path "/home/sjlwa/snap/flutter/common/flutter/")

(setq eshell-prompt-function 'sjlwa-eshell-prompt)
(setq eshell-highlight-prompt nil)

(defun eshell-init-setup ()
  (company-mode -1)
  (setq eshell-hist-ignoredups t))

(add-hook 'eshell-mode-hook 'eshell-init-setup)




 (setq treesit-language-source-alist
       '(
		 (bash "https://github.com/tree-sitter/tree-sitter-bash")
		 (cmake "https://github.com/uyha/tree-sitter-cmake")
		 (c "https://github.com/tree-sitter/tree-sitter-c")
		 (css "https://github.com/tree-sitter/tree-sitter-css")
		 (elisp "https://github.com/Wilfred/tree-sitter-elisp")
		 (go "https://github.com/tree-sitter/tree-sitter-go")
		 (gomod "https://github.com/camdencheek/tree-sitter-go-mod.git")
		 (html "https://github.com/tree-sitter/tree-sitter-html")
		 (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
		 (json "https://github.com/tree-sitter/tree-sitter-json")
		 (make "https://github.com/alemuller/tree-sitter-make")
		 (markdown "https://github.com/ikatyang/tree-sitter-markdown")
		 (python "https://github.com/tree-sitter/tree-sitter-python")
		 (php "https://github.com/tree-sitter/tree-sitter-php.git")
		 (toml "https://github.com/tree-sitter/tree-sitter-toml")
		 (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
		 (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
		 (yaml "https://github.com/ikatyang/tree-sitter-yaml")))


(add-hook 'after-init-hook #'(lambda ()
							   (load "~/dev/emacs-conf/fuzzy.el")
							   (load "~/dev/emacs-conf/bindings.el")
							   (load "~/dev/emacs-conf/misc.el")

							   ;; Package Setup
							   (use-package diff-hl :defer t :ensure t :init (global-diff-hl-mode))
							   ;; (use-package projectile :ensure t :defer t)
							   ;; (use-package magit :ensure t :defer t)
							   ;; (use-package ag :ensure t :defer t)
							   ;; (use-package http :ensure t :defer t)
							   (use-package expand-region :ensure t :defer t :bind (("S-SPC" . er/contract-region) ("C-SPC" . er/expand-region)))
							   (use-package move-text :ensure t :defer t :bind (("M-S-<up>" . move-text-up) ("M-S-<down>" . move-text-down)))
							   ;; (use-package format-alel :ensure t :defer t)
							   ;;(use-package esup :ensure t :pin melpa :config (setq esup-depth 0)) ;; To use MELPA Stable use ":pin melpa-stable",
							   
							   ))

;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ellama rust-mode evil rainbow-mode multiple-cursors php-mode vmd-mode php-quickhelp dockerfile-mode lsp-pyright company-jedi elpy go-mode tree-sitter-langs tree-sitter standard-themes eldoc-overlay eldoc-box yaml-mode websocket web-server typescript-mode smex skewer-mode projectile pdf-tools paredit origami move-text magit lsp-ui lsp-dart list-utils js2-refactor iedit http hover helm format-all flycheck expand-region esup emmet-mode dumb-jump dired-sidebar diff-hl coverlay company-web company-quickhelp clang-format arduino-mode ag)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
