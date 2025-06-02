;;; -*- lexical-binding: t -*-

(defun plantuml-preview-set-default-background ()
  (when (string-equal (buffer-name) plantuml-preview-buffer)
    (buffer-face-set '(:background "azure3"))))

(with-eval-after-load 'plantuml-mode
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

  (add-hook 'image-mode-hook #'plantuml-preview-set-default-background)

  (setq plantuml-jar-path "/home/sjlwa/Downloads/plantuml-1.2025.2.jar"
        plantuml-default-exec-mode 'jar))
