;;; -*- lexical-binding: t -*-

(setq-default bye-buffers-list
              `((seq "*ag search" (+ anything))
                 "*Async-native-compile-log*"
                 "*Buffer List*"
                 "*compilation*"
                 "*Compile-Log*"
                 "*Completions*"
                 "*company-documentation*"
                 (seq "*EGLOT" (+ anything))
                 (seq "*Flymake" (+ anything))
                 (seq "*Flycheck" (+ anything))
                 "*Help*"
                 (seq "magit" (+ anything))
                 "*Messages*"
                 "*Packages*"
                 "*Shell Command Output*"
                 "*vc-diff*"
                 "*Warnings*"
                 "*xref*"
                 ))

(defun bye-buffers-wrap-word (keyword)
  "Wrap KEYWORD string into a pattern of the form (seq (* anything) KEYWORD (* anything))."
  `(seq (* anything) ,keyword (* anything)))

(defun bye-buffers-add (patterns)
  "Destructively append NEW-PATTERNS to BUFFS' patterns using nconc."
  (nconc bye-buffers-list patterns))

(defun bye-buffers-add-inbetween (patterns)
  "Destructively append NEW-PATTERNS to BUFFS' patterns using nconc."
  (bye-buffers-add (mapcar #'bye-buffers-wrap-word patterns)))

(defun bye-buffers-build-regexp ()
  "Return a regexp string matching hidden buffer names."
  (rx-to-string
   `(or (and bos " ")
        (and bos
             (or ,@bye-buffers-list)
             eos))))

(defun bye-buffers-skip-method (window buffer bury-or-kill)
  "Hide special buffers"
  (string-match-p (bye-buffers-build-regexp) (buffer-name buffer)))

(define-minor-mode bye-buffers-mode
  "Toggle visibility of hidden buffers."
  :global t

  (if bye-buffers-mode
      (setq switch-to-prev-buffer-skip 'bye-buffers-skip-method)
    (setq switch-to-prev-buffer-skip nil)))
