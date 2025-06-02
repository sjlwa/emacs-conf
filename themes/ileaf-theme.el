;;; ileaf-theme.el --- A light theme  -*- lexical-binding:t -*-

;;;###theme-autoload
(deftheme ileaf
  :background-mode 'dark
  :kind 'color-scheme)

(custom-theme-set-faces
 'ileaf
 '(default ((t (:background "#1b1b13" :foreground "wheat2"))))
 '(region ((t (:background "Gray20" :foreground "Wheat3"))))
 '(font-lock-builtin-face       ((t (:foreground "LightSteelBlue"))))
 '(font-lock-preprocessor-face  ((t (:foreground "CornFlowerBlue" :italic t))))
 '(font-lock-regexp-grouping-backslash ((t (:bold t :weight bold))))
 '(font-lock-regexp-grouping-construct ((t (:bold t :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "Aquamarine"))))
 '(font-lock-function-name-face ((t (:foreground "white" :weight bold))))
 '(font-lock-string-face        ((t (:foreground "OliveDrab2"))))
 '(font-lock-comment-face       ((t (:italic t :slant oblique :foreground "Gray60"))))
 '(font-lock-doc-face           ((t (:italic t :slant oblique :foreground "DarkSeaGreen1"))))
 '(font-lock-doc-markup-face      ((t (:foregrond "#a98"))))
 '(font-lock-warning-face ((t (:bold t :foreground "Pink" :weight bold))))
 '(cursor ((t (:background "LemonChiffon4"))))
 '(fringe ((t (:background "#161616" :foreground "Wheat"))))
 '(header-line ((t (:box (:line-width -1 :color "grey20" :style released-button) :background "grey20" :foreground "grey90"  :height 0.9))))
 '(help-argument-name ((t (:italic t :slant italic))))

 '(minibuffer-prompt ((t (:foreground "aquamarine1"))))
 '(icomplete-selected-match ((t (:background "aquamarine2" :foreground "black" :weight bold))))
 '(completions-common-part ((t (:width normal :weight normal :slant normal
                                       :foreground "WhiteSmoke" :background "black" :height 0.9))))

 '(mode-line ((t (:background "#333" :foreground "Wheat3"
                  :box (:line-width -1 :style released-button)
                  :height 0.9))))

 '(mode-line-emphasis ((t (:bold t :weight bold))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "grey40"
                                              :style released-button)
                                 :height 0.9))))
 
  '(mode-line-inactive ((t (:background "#222"
                  :box (:line-width -1 :style released-button)
                  :height 0.9)))))

(provide-theme 'ileaf)

;;; ileaf-theme.el ends here
