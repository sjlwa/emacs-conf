(setq my-packages--essentials
      '(ag
        diff-hl
        esup
        expand-region
        http
        magit
        move-text
        projectile))

(setq my-packages--languages
      '(csharp-mode
        dart-mode
        lsp-java
        lsp-pyright
        js2-mode
        web-mode))

(setq my-packages--ide
      '(company
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
        yasnippet))

(setq my-packages--all
      (append my-packages--essentials
              my-packages--languages
              my-packages--ide))


(defun sjlwa-packages-source ()
  "Load the packages archives"
  (interactive)
  ;;(require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (package-initialize))

;; Lazy load extra package sources
(advice-add 'list-packages :before #'sjlwa-packages-source)


(defun sjlwa-packages-install ()
  "Select sets of packages to install."
  (interactive)
  (let ((selected (completing-read "Select packages type: "
                                   '(all essentials languages ide))))
    (let ((selected-packages
           (symbol-value (intern (concat "my-packages--" selected)))))
      (sjlwa/packages-source)
      ;; install selected packages
      (dolist (package selected-packages)
        (unless (package-installed-p package)
          (package-install package)))))
  (prin1 "Done."))


(defun prevent-package-selected-packages-update (&rest args)
  "Prevent package-selected-packages from being written to custom-file."
  (when (boundp 'package-selected-packages)
    (setq package-selected-packages nil)))

(advice-add 'package--save-selected-packages
            :override #'prevent-package-selected-packages-update)
(advice-add 'package-install
            :after #'prevent-package-selected-packages-update)
