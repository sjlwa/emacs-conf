(setq package-selected-packages
      '(;; essentials
        ag
        diff-hl
        esup
        expand-region
        http
        magit
        move-text
        multiple-cursors
        projectile
	    rainbow-delimiters

        ;; languages
        ;; dart-mode
        ;; lsp-java
        ;; lsp-pyright
        js2-mode
        web-mode

	    ;; ide
	    company
        company-quickhelp
        editorconfig
        ellama
        emmet-mode
        esh-autosuggest
        flycheck
        ;; format-all
        ;; llm-ollama
        lsp-mode
        lsp-ui
        ;; skewer-mode
        tree-sitter
        tree-sitter-langs
        yasnippet

	))

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
