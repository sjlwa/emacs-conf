(defun hidden-buffers-regexp ()
  "Define regular expressions for hidden buffers"
      (rx (or (and bos " ")
              (and bos
                   (or (seq "*ag search" (+ anything))
					   "*Async-native-compile-log*"
                       "*Buffer List*"
                       (seq "*codeium" (+ anything))
                       (seq "csharp-roslyn")
                       (seq (+ anything) "csharp-roslyn" (+ anything))
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
                       (seq "*Flycheck" (+ anything))
					   (seq "*gopls" (+ anything))
                       (seq "*html" (+ anything))
                       "*Help*"
                       (seq "*jdtls" (+ anything))
                       (seq "*jsts*" (+ anything))
                       (seq "*lsp-" (+ anything))
                       (seq "magit" (+ anything))
                       "*Messages*"
                       "OmniServer"
                       (seq "*omnisharp" (+ anything))
                       "*Packages*"
                       (seq "*pylsp" (+ anything))
                       (seq "*pyright" (+ anything))
                       (seq "*sonarlint" (+ anything))
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

(defun my-buffer-skip-p (window buffer bury-or-kill)
  "Hide special buffers"
  (string-match-p (hidden-buffers-regexp) (buffer-name buffer)))

(defun fuzzy-mode ()
  (interactive)
  (setq switch-to-prev-buffer-skip 'my-buffer-skip-p)
  (fido-vertical-mode))
