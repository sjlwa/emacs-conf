(defun init-startup ()
  "temporarily increase `gc-cons-hold' when loading to speed up startup."
  (setq gc-cons-threshold most-positive-fixnum
        file-name-handler-alist nil ;; Avoid analyzing files when loading remote files.
        inhibit-startup-screen t
        warning-minimum-level :emergency
        package-enable-at-startup nil)

  (add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000))))

(defun show-init-scratch-message ()
  "Insert initial scratch message directly"
  (with-current-buffer "*scratch*"
    (let* ((message (format "Emacs %s - Hi" emacs-version))
           (width (window-total-width))
           (height (window-total-height))
           (padding-x (make-string (/ (- width (length message)) 5) ?\s))
           (padding-y (make-string (/ (1- height) 4) ?\n)))
      (erase-buffer)
      (insert (concat padding-y padding-x message))
      (put-text-property (point-min) (point-max)
                         'face '(:foreground "SpringGreen" :height 320)))))

(defun set-default-window/frame-config ()
    "Sets the default frame (Use in case not loading from .Xresources)."
    (setq default-frame-alist
          '((width . 90) (height . 35)
			(font . "Iosevka-16")
            (tool-bar-lines . 0)
            (menu-bar-lines . 0)
			(vertical-scroll-bars . nil)))
    (blink-cursor-mode -1))

(defun interactivity-modes-enable/configure ()
  "Enable the modes required for a pleasant interactivity."
  (cua-mode 1)
  (xterm-mouse-mode 1)
  (column-number-mode t)
  (delete-selection-mode +1)
  (global-goto-address-mode +1)
  (electric-pair-mode t)
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  (put 'dired-find-alternate-file 'disabled nil)
  ;;(pixel-scroll-precision-mode 1) ;; Doesn't work when lsp-mode is active
  (global-diff-hl-mode) ;; (add-hook 'after-init-hook 'global-diff-hl-mode)
  (fset 'yes-or-no-p 'y-or-n-p))

(defun set-default-general-directories ()
  "Set directories for general tasks."
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
        auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-saved-files/" t))
        emacs-repo (file-name-directory (file-chase-links "~/.emacs"))))

(defun interactivity-modes-set/configure-vars ()
  (setq dired-kill-when-opening-new-dired-buffer t
        vc-follow-symlinks t
        tooltip-delay 0.1
        eldoc-idle-delay 0
        indent-tabs-mode nil
        initial-major-mode 'fundamental-mode)

  (setq-default indent-tabs-mode nil
                electric-indent-inhibit t
                tab-width 4))

(defun org-mode-define-config ()
  (with-eval-after-load 'org
    (setq org-support-shift-select t
          org-startup-indented t)
    (set-org-mode-tasks-status)
    (add-hook 'org-mode-hook #'visual-line-mode)))

(defun esup-define-init ()
  (with-eval-after-load 'esup
    (setq esup-user-init-file (file-truename "~/.emacs"))
    (setq esup-depth 0)))

(defun sjlwa/window-system ()
  (show-init-scratch-message)
  (load-theme 'modus-vivendi))

(defun load-config-file (filename)
  (load (concat emacs-repo filename)))
