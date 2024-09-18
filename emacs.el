;; temporarily increase `gc-cons-hold' when loading to speed up startup.
(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil ;; Avoid analyzing files when loading remote files.
      inhibit-startup-screen t
	  warning-minimum-level :emergency
      package-enable-at-startup nil)


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
      (load-theme 'tango-dark)
    (xterm-mouse-mode 1)))

;; Enable mouse features
(xterm-mouse-mode 1)

(column-number-mode t)
(delete-selection-mode +1)
(global-goto-address-mode +1)
(electric-pair-mode t)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(put 'dired-find-alternate-file 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

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
      indent-tabs-mode nil
      initial-major-mode 'fundamental-mode)

;; Insert initial scratch message directly
(with-current-buffer "*scratch*"
  (let* ((message (format "Emacs %s - Hi" emacs-version))
         (width (window-total-width))
         (height (window-total-height))
         (padding-x (make-string (/ (- width (length message)) 2) ?\s))
         (padding-y (make-string (/ (1- height) 2) ?\n)))
    (erase-buffer)
    (insert (concat padding-y padding-x message))
    (put-text-property (point-min) (point-max) 'face '(:foreground "yellow"))))



(setq-default indent-tabs-mode nil
              electric-indent-inhibit t
              tab-width 4)

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

(load "~/dev/emacs-conf/fuzzy.el")
(load "~/dev/emacs-conf/pkgconf.el")
(load "~/dev/emacs-conf/ide.el")
(load "~/dev/emacs-conf/langs.el")
(load "~/dev/emacs-conf/commands.el")
(load "~/dev/emacs-conf/bindings.el")


(add-hook 'after-init-hook 'global-diff-hl-mode)

(with-eval-after-load 'esup
  (setq esup-user-init-file (file-truename "~/.emacs"))
  (setq esup-depth 0))
;; (setq esup-init-user-file "/home/sjlwa/dev/emacs-conf/pkgconf.el")


;;(setq eshell-prompt-function 'sjlwa-eshell-prompt)
;;(setq eshell-highlight-prompt nil)
(add-hook 'eshell-mode-hook 'eshell-init-setup)
(add-hook 'after-change-major-mode-hook 'sjlwa/toggle-mode-line-based-on-mode)

(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
