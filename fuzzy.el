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
                       "*company-documentation*"
					   (seq "*css" (+ anything))
                       (seq "*EGLOT" (+ anything))
                       (seq "*Flymake" (+ anything))
					   (seq "*gopls" (+ anything))
                       "*Help*"
                       (seq "*jdtls" (+ anything))
                       (seq "*jsts*" (+ anything))
                       (seq "*lsp-" (+ anything))
                       (seq "magit" (+ anything))
                       "*Messages*"
                       "OmniServer"
                       (seq "*omnisharp" (+ anything))
                       "*Packages*"
                       "*pylsp*"
                       (seq "*pyright" (+ anything))
					   (seq "*iph" (+ anything))
                       ;;"*scratch*"
                       "*Shell Command Output*"
                       (seq "*LSP Dart" (+ anything))
                       (seq "*dart_analysis" (+ anything))
                       (seq "*ts-ls" (+ anything))
                       "*vc-diff*"
					   "*Warnings*"
                       "*xref*"
                       )
                   eos))))

;; Hide special buffers
(defun my-buffer-skip-p (window buffer bury-or-kill)
  (string-match-p hidden-buffers-regexp (buffer-name buffer)))

(setq switch-to-prev-buffer-skip 'my-buffer-skip-p)

(fido-vertical-mode)
