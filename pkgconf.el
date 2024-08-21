(defun sjlwa/load-packages-sources ()
  "Load the packages archives"
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (package-initialize))

;; Lazy load extra package sources
(advice-add 'list-packages :before #'sjlwa/load-packages-sources)



(use-package diff-hl :defer t :ensure t :init (global-diff-hl-mode))
(use-package projectile :ensure t :defer t)
(use-package magit :ensure t :defer t)
(use-package ag :ensure t :defer t)
(use-package http :ensure t :defer t)
(use-package expand-region :ensure t :defer t :bind (("S-SPC" . er/contract-region) ("C-SPC" . er/expand-region)))
(use-package move-text :ensure t :defer t :bind (("M-S-<up>" . move-text-up) ("M-S-<down>" . move-text-down)))
;; (use-package format-alel :ensure t :defer t)
;;(use-package esup :ensure t :pin melpa :config (setq esup-depth 0)) ;; To use MELPA Stable use ":pin melpa-stable",
