;; Initialize package system
(require 'package)

;; Add package archives
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Initialize package system
(package-initialize)

;; Refresh package list if needed
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Configure use-package
(require 'use-package)
(setq use-package-always-ensure t)

;; Basic UI settings
(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(column-number-mode 1)
(global-visual-line-mode t)

;; Automatically reread from disk if the underlying file changes
(setq auto-revert-interval 1)
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Save history of minibuffer
(savehist-mode)

;; Fix archaic defaults
(setq sentence-end-double-space nil)

;; Font
(when (display-graphic-p)
  (set-frame-font "GoMono Nerd Font Mono 11" nil t))

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config))

;; Doom modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Icons
(use-package all-the-icons
  :if (display-graphic-p))

;; Which key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Evil mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; Use visual line motions
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  ;; Avy binding
  (evil-define-key 'normal 'global (kbd "f") 'avy-goto-char-2))

;; Evil collection for better evil integration with various modes
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; Evil surround
(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Avy for quick navigation
(use-package avy
  :ensure t)

;; Load additional configuration from mixins if needed
;; Uncomment these as you confirm they work
(load-file (expand-file-name "mixins/base.el" user-emacs-directory))
(load-file (expand-file-name "mixins/dev.el" user-emacs-directory))
(load-file (expand-file-name "mixins/vim-like.el" user-emacs-directory))
;; (load-file (expand-file-name "mixins/golang.el" user-emacs-directory))
(load-file (expand-file-name "mixins/writing.el" user-emacs-directory))
(load-file (expand-file-name "mixins/org.el" user-emacs-directory))

;; Custom variables set by Emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(avy evil-surround evil-collection evil which-key all-the-icons doom-modeline doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
