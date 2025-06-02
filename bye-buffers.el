;;; -*- lexical-binding: t -*-

(cl-defstruct bye-buffers patterns)

(setq-default hidden-buffers
              (make-bye-buffers
               :patterns
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
                 )))

(defun bye-buffers-wrap-keyword-into-pattern (keyword)
  "Wrap KEYWORD string into a pattern of the form (seq (* anything) KEYWORD (* anything))."
  `(seq (* anything) ,keyword (* anything)))

(cl-defmethod bye-buffers-add-patterns ((buffs bye-buffers) patterns)
  "Destructively append NEW-PATTERNS to BUFFS' patterns using nconc."
  (setf (bye-buffers-patterns buffs)
        (nconc (bye-buffers-patterns buffs) patterns)))

(cl-defmethod bye-buffers-add-patterns-inbetween ((buffs bye-buffers) patterns)
  "Destructively append NEW-PATTERNS to BUFFS' patterns using nconc."
  (bye-buffers-add-patterns buffs (mapcar #'bye-buffers-wrap-keyword-into-pattern patterns)))

(cl-defmethod bye-buffers-build-regexp ((buffs bye-buffers))
  "Return a regexp string matching hidden buffer names."
  (rx-to-string
   `(or (and bos " ")
        (and bos
             (or ,@(bye-buffers-patterns buffs))
             eos))))

(defun bye-buffers-skip-method (window buffer bury-or-kill)
  "Hide special buffers"
  (string-match-p (bye-buffers-build-regexp hidden-buffers) (buffer-name buffer)))

(define-minor-mode bye-buffers-mode
  "Toggle visibility of hidden buffers."
  :lighter "byebuf"
  :global t

  (if bye-buffers-mode
      (setq switch-to-prev-buffer-skip 'bye-buffers-skip-method)
    (setq switch-to-prev-buffer-skip nil)))
