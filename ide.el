(defun company-init-config ()
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-show-numbers t
        company-tooltip-width-grow-only t
        company-tooltip-maximum-width 40
        company-tooltip-minimum-width 40
        company-tooltip-offset-display 'lines
        company-tooltip-align-annotations nil)
  
  (unless (eq major-mode 'eshell-mode)
    (setq company-transformers '(;; delete-consecutive-dups ;; TODO: Fix crash eshell with esh-autocomplete.
                                 company-sort-by-occurrence)))

  (if (require 'company-quickhelp nil 'noerror)
      (company-quickhelp-mode)))

(defun lsp-mode-init-config ()
  (setq lsp-apply-edits-after-file-operations nil
        lsp-enable-file-watchers nil
        lsp-diagnostics-flycheck-default-level 'info
        lsp-ui-sideline-diagnostic-max-line-length 150
        lsp-ui-sideline-diagnostic-max-lines 3
        lsp-completion-show-detail nil
        lsp-auto-guess-root t))

(bye-buffers-add-inbetween '("*lsp-" "*sonarlint" "*codeium"))

(add-hook 'lsp-mode-hook 'lsp-mode-init-config)
(eval-after-load 'yasnippet '(yas-global-mode))
(eval-after-load 'editorconfig '(editorconfig-mode 1))

(add-to-list 'auto-mode-alist '("\\.yml?\\'" . yaml-ts-mode))

(add-to-list 'auto-mode-alist '("\\.http?\\'" . http-mode))


(setq grip-binary-path "/usr/bin/grip")
;; (add-hook 'markdown-mode-hook #'grip-mode)
;; (add-hook 'org-mode-hook #'grip-mode)
(setq grip-url-browser "firefox")
(setq grip-update-after-change nil)



;; (with-eval-after-load 'lsp-sonarlint
;;   (setq lsp-sonarlint-download-url
;;         (concat
;;          "https://github.com/SonarSource/sonarlint-vscode/releases/download/"
;;          "4.12.0%2B76892/sonarlint-vscode-linux-x64-4.12.0.vsix"))
;;   ;; (setq lsp-sonarlint-enabled-analyzers '(omnisharp cfamily))
;;   (add-to-list 'lsp-sonarlint-modes-enabled csharp)) csharp-roslyn solves it


;; (add-to-list 'eglot-server-programs
;;              `(csharp-mode . ("/usr/lbin/omnisharp" "-lsp")))


;; (load-file "~/.emacs.d/codeium.el/codeium.el")
;; (use-package codeium
;;   :init
;;   (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
;;   :config
;;     (setq use-dialog-box nil)
;;   )



(defun ellama-init-config ()
  (setq ellama-language "English"
        ellama-provider (make-llm-ollama
                         :chat-model "llama3.1"
                         :embedding-model "llama3.1")))

(eval-after-load 'ellama #'ellama-init-config)


;; (load-file "~/.emacs.d/emacs-copilot/copilot.el")
;; (defun copilot-complete ()
;;   (interactive)
;;   (let* ((spot (point))
;;          (inhibit-quit t)
;;          (curfile (buffer-file-name))
;;          (cash (concat curfile ".cache"))
;;          (hist (concat curfile ".prompt"))
;;          (lang (file-name-extension curfile))

;;          ;; extract current line, to left of caret
;;          ;; and the previous line, to give the llm
;;          (code (save-excursion
;;                  (dotimes (i 2)
;;                    (when (> (line-number-at-pos) 1)
;;                      (previous-line)))
;;                  (beginning-of-line)
;;                  (buffer-substring-no-properties (point) spot)))

;;          ;; create new prompt for this interaction
;;          (system "\
;; You are an Emacs code generator. \
;; Writing comments is forbidden. \
;; Writing test code is forbidden. \
;; Writing English explanations is forbidden. ")
;;          (prompt (format
;;                   "[INST]%sGenerate %s code to complete:[/INST]\n```%s\n%s"
;;                   (if (file-exists-p cash) "" system) lang lang code)))

;;     ;; iterate text deleted within editor then purge it from prompt
;;     (when kill-ring
;;       (save-current-buffer
;;         (find-file hist)
;;         (dotimes (i 10)
;;           (let ((substring (current-kill i t)))
;;             (when (and substring (string-match-p "\n.*\n" substring))
;;               (goto-char (point-min))
;;               (while (search-forward substring nil t)
;;                 (delete-region (- (point) (length substring)) (point))))))
;;         (save-buffer 0)
;;         (kill-buffer (current-buffer))))

;;     ;; append prompt for current interaction to the big old prompt
;;     (write-region prompt nil hist 'append 'silent)

;;     ;; run llamafile streaming stdout into buffer catching ctrl-g
;;     (with-local-quit
;;       (call-process "~/libs/llava-v1.5-7b-q4.llamafile"
;;                     nil (list (current-buffer) nil) t
;;                     "--prompt-cache" cash
;;                     "--prompt-cache-all"
;;                     "--silent-prompt"
;;                     "--temp" "0"
;;                     "-c" "1024"
;;                     "-ngl" "35"
;;                     "-r" "```"
;;                     "-r" "\n}"
;;                     "-f" hist))

;;     ;; get rid of most markdown syntax
;;     (let ((end (point)))
;;       (save-excursion
;;         (goto-char spot)
;;         (while (search-forward "\\_" end t)
;;           (backward-char)
;;           (delete-backward-char 1 nil)
;;           (setq end (- end 1)))
;;         (goto-char spot)
;;         (while (search-forward "```" end t)
;;           (delete-backward-char 3 nil)
;;           (setq end (- end 3))))

;;       ;; append generated code to prompt
;;       (write-region spot end hist 'append 'silent))))







;;(load "~/.emacs.d/eglot-booster/eglot-booster.el")
;; (use-package eglot-booster
;; 	:after eglot
;; 	:config	(eglot-booster-mode))

;;(use-package eglot
;;  :custom
;;  (fset #'jsonrpc--log-event #'ignore)
;;  (setf (plist-get eglot-events-buffer-config :size) 0))
