;;; -*- lexical-binding: t -*-

(defun dart-init-config ()
  (setq lsp-dart-sdk-dir (concat flutter-sdk-path "bin/cache/dart-sdk")
        lsp-dart-flutter-sdk flutter-sdk-path
        flutter-sdk-path "~/snap/flutter/common/flutter/"))

(defun load-lang--dart ()
  (add-hook 'dart-mode-hook 'lsp-deferred)
  (eval-after-load 'dart-mode '(dart-init-config))
  (bye-buffers-add-inbetween '("dart_analysis" "LSP Dart")))
