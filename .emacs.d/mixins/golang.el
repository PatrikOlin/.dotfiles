;;; silver computing machine
;;;
;;; Mixin: Golang

;;; Usage: Append or require this file from init.el for Go language support.

;;; Contents:
;;;
;;;  - Core Packages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Core Packages
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  (setq gofmt-command "goimports")
  (setq on-save-gofmt t))

(add-hook 'go-mode-hook 'flymake-mode)
(add-hook 'go-ts-mode-hook 'flymake-mode)
