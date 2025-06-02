(defun image-zoom-in ()
  (interactive)
  (image-transform-set-scale (* image-transform-scale 1.2)))

(defun image-zoom-out ()
  (interactive)
  (image-transform-set-scale (/ image-transform-scale 1.2)))

(with-eval-after-load 'image-mode
  (define-key image-mode-map (kbd "C-+") #'image-zoom-in)
  (define-key image-mode-map (kbd "C--") #'image-zoom-out)
  (define-key image-mode-map (kbd "C-0") #'image-transform-reset))
