;;; clipboard --- clipboard -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun clipboard-copy (text)
  (with-temp-buffer
    (insert text)
    (call-process-region
     (point-min) (point-max)
     "xclip" nil nil nil "-sel" "clip")
    (message "Copied: %s" text)))

(defun clipboard-pwd ()
  "Copy the current directory path"
  (interactive)
  (clipboard-copy default-directory))

(defun clipboard-current-filepath ()
  "Copy the current directory path"
  (interactive)
  (clipboard-copy (buffer-file-name)))

(provide 'clipboard)

;;; clipboard.el ends here
