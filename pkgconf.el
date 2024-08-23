(add-hook 'after-init-hook 'global-diff-hl-mode)

(use-package projectile :defer t)
(use-package magit :defer t)
(use-package ag :defer t)
(use-package http :defer t)
(use-package expand-region :defer t)
(use-package move-text :defer t)
(use-package format-alel :defer t)

(with-eval-after-load 'esup
  (setq esup-user-init-file (file-truename "~/.emacs"))
  (setq esup-depth 0))
