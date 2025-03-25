(require 'subr-x)

(defun current-git-branch ()
  "Get the current branch name of the current repository."
  (with-temp-buffer
    (when (= 0 (call-process "git" nil t nil "branch" "--show-current"))
      (string-trim (buffer-string)))))

(defun with-face (str &rest face-plist)
  (propertize str 'face face-plist))

(defun sjlwa/read-file-str (filename)
  "Return the contents of a file"
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun sjlwa/eval-file (filename)
  "Evaluate the contents of FILENAME and print the result or error."
  (condition-case err
      (let ((content (sjlwa/read-file-str filename)))
        (let* ((forms (read (concat "(progn " content ")")))
               (result (eval forms)))))
    (error
     (princ (format "Error %s: %S\n" filename err)))))

(defun sjlwa/resize-minibuffer-full-window ()
  "Set window size of minibuffer to its max size"
  (delete-other-windows)
  (let ((minibuffer-window (minibuffer-window)))
    (select-window minibuffer-window)
    (window-resize minibuffer-window (frame-height))))

(defun msg (message)
  (if (not inhibit-messages) (message message)))

(defun merge-alists-simple (&rest alists)
  "Combine multiple ALISTS keeping duplicates."
  (apply #'append alists))

(defun extract-true-keys (alist)
  "Extract keys with an asociated t value from an alist."
  (delq nil
        (mapcar (lambda (pair)
                  (when (eq (cdr pair) t)
                    (car pair)))
                alist)))

(defun eshell-call-command-as-compilation (command args)
  "Run COMMAND with ARGS as a compilation process."
  (compile (concat command " " (string-join args " "))))
