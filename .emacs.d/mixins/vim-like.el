;;; Emacs Bedrock
;;;
;;; Mixin: Vim emulation

;;; Usage: Append or require this file from init.el for bindings in Emacs.

;;; Contents:
;;;
;;;  - Core Packages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Core Packages
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evil: vi emulation
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-keybinding nil)       ; prep to load evil-collection
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Configuring initial major mode for some modes
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; Evil-surround: vim surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(global-unset-key (kbd "C-SPC"))
;; General.el
(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer silver_computing_machine/leader-keys
    :prefix "SPC"
    :states '(normal visual motion emacs)
    :keymaps 'override
    :global-prefix "C-SPC")

  (silver_computing_machine/leader-keys
    "f" '(:ignore t :which-key "files")
    "ff" '(consult-fd :which-key "find files")
    "fr" '(counsel-recentf :which-key "recent files")
    "p" '(:ignore t :which-key "project")
    "pp" '(projectile-switch-project :which-key "switch project")
    "pa" '(projectile-add-known-project :which-key "add project")
    "ps" '(projectile-rg :which-key "search project")
    "a" '(:ignore t :which-key "avy")
    "aa" '(avy-goto-char :which-key "goto char")
    "as" '(silver_computing_machine/avy-goto-char :which-key "goto char")'
    "al" '(avy-goto-line :which-key "goto line")
    "b" '(:ignore t :which-key "buffer")
    "bs" '(consult-buffer :which-key "consult buffer")
    "bk" '(kill-buffer :which-key "kill buffer") 
    "bq" '(kill-current-buffer :which-key "kill current buffer")
    "e" '(:ignore t :which-key "embark")
    "ea" '(embark-act :which-key "embark act")
    "l" '(:ignore t :which-key "lsp")
    "la" '(eglot-code-actions :which-key "eglot code actions")
    "lr" '(eglot-rename :which-key "eglot rename")
    "lf" '(eglot-format :which-key "eglot format")
    "w" '(:ignore t :which-key "window")
    "ww" '(other-window :which-key "other window")
    "wq" '(delete-window :which-key "delete window")
    "wk" '(delete-window :which-key "delete window")
    "wD" '(delete-other-windows :which-key "delete other windows")
    "ws" '(split-window-below :which-key "split window below")
    "wS" '(split-window-right :which-key "split window right")
    "w-" '(split-window-below :which-key "split window below")
    "w|" '(find-file :which-key "find file")
    "c"  '(find-file user-emacs-directory :which-key "find emacs config files")
    ))

;; Hydra
(use-package hydra
  :ensure t)

(defhydra hydra-text-scale (:color blue :timeout 6)
  "Text scale: %(text-scale-mode-line)"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("0" (text-scale-set 0) "reset")
  ("q" nil "quit" :color blue))

;; Custom Avy function that places the cursor after the search string
(defun silver_computing_machine/avy-goto-char ()
  "Read one or many consecutive chars with 'avy-goto-char-timer and jump after the search string."
  (interactive)
  (unless (eq (avy-goto-char-timer) t)
    (forward-char (length avy-text))))
