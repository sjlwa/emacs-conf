;;; package --- icomplete-extra -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'icomplete
  (setq completion-ignore-case t
        completion-styles '(flex basic)
        completions-sort 'alphabetical
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t))

(fido-vertical-mode)

(provide 'icomplete-extra)

;;; icomplete-extra.el ends here
