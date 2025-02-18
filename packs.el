(defun get-packages-essentials ()
  '((ag . t)
    (clipetty . t)
    (xclip . t)
    (diff-hl . t)
    (esup . t)
    (expand-region . t)
    (http . t)
    (magit . t)
    (move-text . t)
    (multiple-cursors . t)
    (projectile . t)
    (rainbow-delimiters . t)
    (auto-sudoedit-mode . t)
    ))

(defun get-packages-languages ()
  '((csproj-mode . t)
    (dart-mode . nil)
    (lsp-java . t)
    (lsp-pyright . nil)
    (js2-mode . t)
    (omnisharp . nil)
    (web-mode . t)))

(defun get-packages-ide ()
  '((company . t)
    (company-quickhelp . t)
    (editorconfig . t)
    (ellama . nil)
    (emmet-mode . t)
    (esh-autosuggest . nil)
    (flycheck . t)
    (format-all . nil)
    (grip-mode . t)
    (llm-ollama . nil)
    (lsp-mode . t)
    (lsp-sonarlint . t)
    (lsp-ui . t)
    (skewer-mode . nil)
    (tree-sitter . t)
    (tree-sitter-langs . t)
    (yasnippet . t)))

(setq package-selected-packages
      (extract-true-keys
       (merge-alists-simple
        (get-packages-essentials)
        (get-packages-languages)
        (get-packages-ide))))

(defun sjlwa-packages-source ()
  "Load the packages archives"
  (interactive)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (package-initialize))

;; Lazy load extra package sources
(advice-add 'list-packages :before #'sjlwa-packages-source)

(advice-add 'package-install-selected-packages :before #'sjlwa-packages-source)

(defun prevent-package-selected-packages-update (&rest args)
  "Prevent package-selected-packages from being written to custom-file."
  (when (boundp 'package-selected-packages)
    (setq package-selected-packages nil)))

(advice-add 'package--save-selected-packages
            :override #'prevent-package-selected-packages-update)
(advice-add 'package-install
            :after #'prevent-package-selected-packages-update)
