;;; package --- keymaps -*- lexical-binding: t -*-
;;; Code:

(defun keymap-global-bind-navigation ()
  "Bind keys for navigation and window movement."
  (keymap-global-set "M-<left>"  #'windmove-left)
  (keymap-global-set "M-<right>" #'windmove-right)
  (keymap-global-set "M-<up>"    #'windmove-up)
  (keymap-global-set "M-<down>"  #'windmove-down)
  (keymap-global-set "C-l"  #'goto-line)
  ;; TODO: fix end/beginning of line
  ;; (keymap-global-set "<s-right>" #'end-of-line)
  ;; (keymap-global-set "<s-left>"  #'beginning-of-line)

  (dotimes (i 10)
    (global-set-key (kbd (format "M-%d" i))
                    (eval `(lambda () (interactive) (tab-select-by-number ,i))))))

(defun keymap-global-bind-text-editing ()
  "Bind keys for text deletion and editing."
  (keymap-global-set "C-<backspace>" #'ryanmarcus/backward-kill-word)
  (keymap-global-set "C-h"           #'ryanmarcus/backward-kill-word)
  (keymap-global-set "M-<backspace>" #'xahlee/backward-delete-word)
  (keymap-global-set "C-k"           #'xahlee/delete-line)
  (keymap-global-set "M-d"           #'xahlee/delete-word)
  (keymap-global-set "C-S-k"         #'xahlee/delete-line-backward)
  ;; (define-key input-decode-map "\C-h" [C-backspace])
  (global-set-key [backtab] 'sjlwa/tab)
  (global-set-key "\t" 'indent-rigidly))

(defun keymap-global-bind-visual ()
  "Bind keys for text scaling and visibility toggles."
  (keymap-global-set "C-+"        #'text-scale-increase)
  (keymap-global-set "C--"        #'text-scale-decrease)
  (keymap-global-set "C-c SPC"    #'hs-toggle-hiding)
  (keymap-global-set "M-S-<up>"   #'move-text-up)
  (keymap-global-set "M-S-<down>" #'move-text-down))

(defun keymap-global-bind-text-selection ()
  "Bind keys for text selection and expansion."
  (global-set-key (kbd "C-a")   #'mark-whole-buffer)
  (global-set-key (kbd "C-SPC") #'er/expand-region)
  (global-set-key (kbd "S-SPC") #'er/contract-region)
  (global-set-key (kbd "C-{")   #'mc/mark-all-in-region)
  (global-set-key (kbd "C-}")   #'mc/mark-next-like-this))

(defun keymap-global-bind-general ()
  "Bind keys for general actions and commands."
  (global-set-key (kbd "ESC ESC ESC") #'esc-esc-esc)
  (global-set-key (kbd "C-w")         #'kill-current-buffer)
  (global-set-key (kbd "C-p")         #'sjlwa/find-file)
  (global-set-key (kbd "C-o")         #'bs-show)
  (global-set-key (kbd "C-S-s")       #'ag)
  ;; (global-set-key (kbd "C-s")         #'save-buffer)
  (global-set-key (kbd "C-z")         #'undo-only)
  (global-set-key (kbd "C-S-z")       #'undo-redo)
  (global-set-key (kbd "C-r")         #'replace-string)
  (global-set-key (kbd "C-c c")       #'project-compile))

(defun keymap-global-load ()
  "Load all global key bindings."
  (keymap-global-bind-navigation)
  (keymap-global-bind-text-editing)
  (keymap-global-bind-visual)
  (keymap-global-bind-text-selection)
  (keymap-global-bind-general))

(with-eval-after-load 'prog-mode
  (define-key prog-mode-map (kbd "S-SPC") 'lsp-execute-code-action))

(provide 'keymaps)

;;; keymaps.el ends here
