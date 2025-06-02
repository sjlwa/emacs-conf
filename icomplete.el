(defun icomplete-force-complete-custom ()
  "Force completion of the current icomplete candidate."
  (interactive)
  (when (bound-and-true-p icomplete-mode)
    (let ((icomplete-prospects-height 1))
      (icomplete-force-complete))))

(with-eval-after-load 'icomplete
  (define-key icomplete-minibuffer-map (kbd "TAB") #'icomplete-force-complete-custom))

(fido-vertical-mode)
