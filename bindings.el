(global-set-key [backtab] 'sjlwa/tab)
(global-set-key "\t" 'indent-rigidly)

(setq
 key-maps-list
 '(
   ("C-w" . sjlwa/ctrl_w)
   ("C-z"   . undo-only)
   ("C-S-z" . undo-redo)
   ("C-p" . sjlwa/find-file)
   ("C-o" . bs-show)
   ("C-S-s" . ag)
   ("C-+" . text-scale-increase)
   ("C--" . text-scale-decrease)
   ("M-<left>"  . windmove-left)
   ("M-<right>" . windmove-right)
   ("M-<up>"    . windmove-up)
   ("M-<down>"  . windmove-down)
   ("C-a"   . mark-whole-buffer)
   ("C-SPC" . er/expand-region)
   ("S-SPC" . er/contract-region)
   ("C-{"   . mc/mark-all-in-region)
   ("C-}"   . mc/mark-next-like-this)
   ("M-S-<up>"   . move-text-up)
   ("M-S-<down>" . move-text-down)
   ("<s-left>"  . beginning-of-line)
   ("<s-right>" . end-of-line)
   ("C-c SPC" . hs-toggle-hiding)
   ("C-r" . replace-string)
   ("C-S-k" . xahlee/delete-line-backward)
   ("C-k" . xahlee/delete-line)
   ("M-d" . xahlee/delete-word)
   ("<M-backspace>" . xahlee/backward-delete-word)
   ("C-<backspace>" . ryanmarcus/backward-kill-word)
   ("C-S-t" . sjlwa/tab-new)
   ("M-0" . (lambda () (interactive) (sjlwa/select-tab-by-number 0)))
   ("M-1" . (lambda () (interactive) (sjlwa/select-tab-by-number 1)))
   ("M-2" . (lambda () (interactive) (sjlwa/select-tab-by-number 2)))
   ("M-3" . (lambda () (interactive) (sjlwa/select-tab-by-number 3)))
   ("M-4" . (lambda () (interactive) (sjlwa/select-tab-by-number 4)))
   ("M-5" . (lambda () (interactive) (sjlwa/select-tab-by-number 5)))
   ("M-6" . (lambda () (interactive) (sjlwa/select-tab-by-number 6)))
   ("M-7" . (lambda () (interactive) (sjlwa/select-tab-by-number 7)))
   ("M-8" . (lambda () (interactive) (sjlwa/select-tab-by-number 8)))
   ("M-9" . (lambda () (interactive) (sjlwa/select-tab-by-number 9)))
   ("C-d" . sjlwa/eshell-tab-exit-close)
   ("ESC ESC ESC" . sjlwa/esc-esc-esc)))

(mapc (lambda (pair)
        (global-set-key (kbd (car pair)) (cdr pair)))
      key-maps-list)

(define-key icomplete-minibuffer-map (kbd "TAB") #'my-icomplete-force-complete)
