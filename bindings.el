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

(defun backward-delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(global-set-key (kbd "C-<backspace>") 'backward-delete-word)


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
	
