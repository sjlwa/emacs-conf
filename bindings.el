;; Tab behaviour
(defun sjlwa/tab ()
  (interactive)
  ;; emmet expansion
  (let ((last-char-before-nil
         (not (or (eq (char-before) 9) ;; tab
                  (eq (char-before) 10) ;; new line
                  (eq (char-before) 32))))) ;; space
    (cond ((and last-char-before-nil (eq major-mode 'web-mode))
           (emmet-expand-line nil)))))

(global-set-key [backtab] 'sjlwa/tab)
(global-set-key "\t" 'indent-rigidly)


;; Files / buffers
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(global-set-key (kbd "C-w") 'kill-buffer)
(global-set-key (kbd "C-z") 'undo-only)
(global-set-key (kbd "C-S-z") 'undo-redo)

;; recursive search
(global-set-key (kbd "C-S-s") 'ag)

;; (global-set-key (kbd "C-s") 'occur)

;; Scale text
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; (setq org-agenda-directory '("~/OrgAgenda"))
;; (global-set-key (kbd "C-c a") 'org-agenda)

(global-set-key (kbd "<s-left>") 'beginning-of-line)
(global-set-key (kbd "<s-right>") 'end-of-line)


(global-set-key (kbd "C-c SPC") 'hs-toggle-hiding)
	
(global-set-key (kbd "C-M-<return>") 'mc/mark-all-in-region)
;;(global-set-key (kbd "C-M-<down>") 'mc/mark-next-like-this)

(global-set-key (kbd "C-<backspace>") 'ryanmarcus/backward-kill-word)

(global-set-key (kbd "ESC ESC ESC") 'sjlwa/esc-esc-esc)

(global-set-key (kbd "C-w") 'sjlwa/ctrl_w)

(global-set-key (kbd "C-S-t") 'sjlwa/tab-new)

(global-set-key (kbd "M-0") (lambda () (interactive) (sjlwa/select-tab-by-number 0)))
(global-set-key (kbd "M-1") (lambda () (interactive) (sjlwa/select-tab-by-number 1)))
(global-set-key (kbd "M-2") (lambda () (interactive) (sjlwa/select-tab-by-number 2)))
(global-set-key (kbd "M-3") (lambda () (interactive) (sjlwa/select-tab-by-number 3)))
(global-set-key (kbd "M-4") (lambda () (interactive) (sjlwa/select-tab-by-number 4)))
(global-set-key (kbd "M-5") (lambda () (interactive) (sjlwa/select-tab-by-number 5)))
(global-set-key (kbd "M-6") (lambda () (interactive) (sjlwa/select-tab-by-number 6)))
(global-set-key (kbd "M-7") (lambda () (interactive) (sjlwa/select-tab-by-number 7)))
(global-set-key (kbd "M-8") (lambda () (interactive) (sjlwa/select-tab-by-number 8)))
(global-set-key (kbd "M-9") (lambda () (interactive) (sjlwa/select-tab-by-number 9)))
;; TODO: bind M- 0 9 as loop
;; (dotimes (i 9)
;;   (global-set-key (kbsd "<M-down>")
;;                   (lambda () (interactive)
;;                     (sjlwa/select-tab-by-number__event-handler))))


(define-key icomplete-minibuffer-map (kbd "TAB") #'my-icomplete-force-complete)
