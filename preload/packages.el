;;; package --- packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(defconst selected-packages
  '(
    ;; essentials
    (ag . t)
    (clipetty . t)
    (xclip . t)
    (diff-hl . t)
    (esup . t)
    (expand-region . t)
    (http . t)
    (magit . t)
    (move-text . t)
    (multiple-cursors . t)
    (projectile . f)
    (rainbow-delimiters . t)
    (auto-sudoedit-mode . t)
    (marginalia . nil)

    ;; languages
    (csproj-mode . t)
    (dart-mode . nil)
    (lsp-java . t)
    (lsp-pyright . nil)
    (js2-mode . t)
    (jtsx . t)
    (omnisharp . nil)
    (web-mode . t)

    ;; ide
    (android-mode . nil)
    (colorful-mode . t)
    (company . t)
    (company-box . nil)
    (company-quickhelp . t)
    (copilot . t)
    (copilot-chat . t)
    (dape . t)
    (editorconfig . t)
    (ellama . nil)
    (emmet-mode . t)
    (esh-autosuggest . nil)
    (format-all . nil)
    (grip-mode . t)
    (js-comint . t)
    (llm-ollama . nil)
    (lsp-mode . t)
    (lsp-sonarlint . t)
    (lsp-ui . t)
    (skewer-mode . nil)
    (tree-sitter . t)
    (tree-sitter-langs . t)
    (treesit-auto . t)
    (yasnippet . t)
    (yasnippet-snippets . t)))

(setq package-selected-packages
      (mapcar #'car
              (seq-filter #'cdr selected-packages)))

(defun prevent-package-selected-packages-update (&rest args)
  "Prevent package-selected-packages from being written to custom-file."
  (when (boundp 'package-selected-packages)
    (setq package-selected-packages nil)))

(advice-add 'package--save-selected-packages
            :override #'prevent-package-selected-packages-update)

(advice-add 'package-install
            :after #'prevent-package-selected-packages-update)

(provide 'packages)

;;; packages.el ends here
