(global-set-key [backtab] 'sjlwa/tab)
(global-set-key "\t" 'indent-rigidly)


(mapc (lambda (pair)
        (global-set-key (kbd (car pair)) (cdr pair)))
      '(

        ;; Files / buffers
        ("C-w" . kill-buffer)
        ("C-z" . undo-only)
        ("C-S-z" . undo-redo)
        ;; ("C-x C-r" . 'recentf-open-files)

        ;; recursive search
        ("C-S-s" . ag)

        ;; scale text
        ("C-+" . text-scale-increase)
        ("C--" . text-scale-decrease)

        ("C-a" . mark-whole-buffer)

        ("<s-left>" . beginning-of-line)
        ("<s-right>" . end-of-line)

        ("C-c SPC" . hs-toggle-hiding)

        ("C-M-<return>" . mc/mark-all-in-region)
        ;;("C-M-<down>" . mc/mark-next-like-this)

        ("C-<backspace>" . ryanmarcus/backward-kill-word)

        ("ESC ESC ESC" . sjlwa/esc-esc-esc)

        ("C-w" . sjlwa/ctrl_w)

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

        ))

(define-key icomplete-minibuffer-map (kbd "TAB") #'my-icomplete-force-complete)

;??????
;;(global-set-key (kbd "C-c") 'kill-ring-save)
;; (add-hook 'term-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-c") 'kill-ring-save)))
