;; Hidden buffers regexp
(setq hidden-buffers-regexp
      (rx (or (and bos " ")
              (and bos
                   (or (seq "*ag search" (+ anything))
					   "*Async-native-compile-log*"
                       "*Buffer List*"
                       "*clang-error*"
                       "*clang-output*"
                       (seq "*clangd" (+ anything))
                       "*compilation*"
                       "*Compile-Log*"
                       "*Completions*"
                       (seq "*EGLOT" (+ anything))
                       (seq "*Flymake" (+ anything))
					   (seq "*gopls" (+ anything))
                       "*Help*"
                       (seq "*lsp-" (+ anything))
                       (seq "magit" (+ anything))
                       "*Messages*"
                       "*Packages*"
                       "*pylsp*"
					   (seq "*iph" (+ anything))
                       ;;"*scratch*"
                       "*Shell Command Output*"
                       (seq "*LSP Dart" (+ anything))
                       (seq "*dart_analysis" (+ anything))
                       (seq "*ts-ls" (+ anything))
                       "*vc-diff*"
					   "*Warnings*")
                   eos))))

;; Hide special buffers
(defun my-buffer-skip-p (window buffer bury-or-kill)
  (string-match-p hidden-buffers-regexp (buffer-name buffer)))

(setq switch-to-prev-buffer-skip 'my-buffer-skip-p)

(use-package fido-mode
  :init (fido-vertical-mode)
  :bind (("C-o" . bs-show)
		 ;;("C-o" . switch-to-buffer)
         ("C-p" . my-find-file)))
