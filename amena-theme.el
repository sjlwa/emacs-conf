;;;###theme-autoload
(deftheme amena
  :background-mode 'dark
  :kind 'color-scheme)

(custom-theme-set-faces
 'amena
 '(default ((t (:background "#22241f" :foreground "white"))))

 ;; Font lock faces
 '(font-lock-builtin-face ((t (:foreground "orchid"))))
 '(font-lock-comment-face ((t (:foreground "#767283" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "LightSalmon2"))))
 '(font-lock-function-name-face ((t (:foreground "#ff996f"))))
 '(font-lock-keyword-face ((t (:foreground "#ffff0b"))))
 '(font-lock-string-face ((t (:foreground "#f57bf5"))))
 '(font-lock-type-face ((t (:foreground "#9fcb66"))))
 '(font-lock-variable-name-face ((t (:foreground "#4ac964"))))
 '(font-lock-warning-face ((t (:foreground "#ff4242" :weight bold))))
 ;;
 '(minibuffer-prompt ((t (:foreground "YellowGreen"))))
 '(icomplete-first-match ((t (:foreground "deep sky blue" :weight bold))))
 '(ido-first-match ((t (:foreground "orange" :weight bold))))
 '(ido-only-match ((t (:foreground "orange" :weight bold))))
 '(ido-subdir ((t (:foreground "MistyRose4" :weight normal))))
 '(region ((t (:background "#255b00" :foreground "white"))))
 '(mode-line ((t (:background "#2a302a" :box (:line-width 1 :color "#504a4a")))))
 ;;
 '(magit-diff-added ((t (:background "#003333"))))
 '(magit-diff-added-highlight ((t (:background "#115555"))))
 '(magit-diff-removed ((t (:background "#331122"))))
 '(magit-diff-removed-highlight ((t (:background "#442233"))))
 )

(provide-theme 'amena)

;;; amena-theme.el ends here
