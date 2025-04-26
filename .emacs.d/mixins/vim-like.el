;;; silver_computing_machine
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

(setq evil-want-keybinding nil)

;; Evil: vi emulation
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-keybinding nil)       ; prep to load evil-collection
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-define-key 'normal 'global (kbd "f") 'avy-goto-char-2)

  ;; Configuring initial major mode for some modes
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Evil-surround: vim surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; Evil-commentary: vim commentary
(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

;; ;; evil-mc: multiple cursors
;; (use-package evil-mc
;;   :ensure t
;;   :init
;;   (global-evil-mc-mode 1))

;; evil-multiedit: multiple cursors
(use-package evil-multiedit
  :ensure t
  :init
  ;;(evil-multiedit-default-keybinds)
  )

(global-unset-key (kbd "C-SPC"))
;; General.el
(use-package general
  :ensure t
  :config
  (general-evil-setup t)

  (general-create-definer silver_computing_machine/leader-keys
    :prefix "SPC"
    :states '(normal visual motion emacs)
    :keymaps 'override
    :global-prefix "C-SPC")

  (silver_computing_machine/leader-keys
    "f" '(:ignore t :which-key "files")
    "ff" '(consult-find :which-key "find files")
    "fg" '(consult-ripgrep :which-key "find text")
    "fr" '(consult-recent :which-key "recent files")
    "p" '(:ignore t :which-key "project")
    "pp" '(projectile-switch-project :which-key "switch project")
    "pa" '(projectile-add-known-project :which-key "add project")
    "pf" '(projectile-rg :which-key "find in project with ripgrep")
    "ps" '(persp-switch :which-key "switch perspective")
    "pb" '(persp-ibuffer :which-key "perspective ibuffer")
    "pk" '(persp-kill :which-key "perspective kill")
    "a" '(:ignore t :which-key "avy")
    "aa" '(avy-goto-char :which-key "goto char")
    "as" '(silver_computing_machine/avy-goto-char :which-key "goto char")'
    "al" '(avy-goto-line :which-key "goto line")
    "b" '(:ignore t :which-key "buffer")
    "bs" '(consult-buffer :which-key "consult buffer")
    "bq" '(kill-buffer :which-key "kill buffer") 
    "bk" '(kill-current-buffer :which-key "kill current buffer")
    "e" '(:ignore t :which-key "embark")
    "ea" '(embark-act :which-key "embark act")
    "l" '(:ignore t :which-key "lsp")
    "la" '(eglot-code-actions :which-key "eglot code actions")
    "lr" '(eglot-rename :which-key "eglot rename")
    "lf" '(eglot-format :which-key "eglot format")
    "le" '(consult-flymake :which-key "consult flymake")
    "m" '(hydra-multi-cursors/body :which-key "multiple cursors")
    "g" '(hydra-magit/body :which-key "hydra magit")
    ;; "g" '(:ignore t :which-key "magit")
    ;; "gg" '(magit :which-key "magit")
    ;; "gs" '(magit-status :which-key "magit status")
    ;; "gd" '(magit-diff :which-key "magit diff")
    ;; "gl" '(magit-log :which-key "magit log")
    ;; "gc" '(magit-commit :which-key "magit commit")
    ;; "gp" '(magit-push :which-key "magit push")
    ;; "gh" '(hydra-magit/body :which-key "hydra magit")
    "t" '(hydra-text-scale/body :which-key "hydra text scale")
    "y" '(:ignore t :which-key "yank")
    "yr" '(avy-copy-region :which-key "avy copy region")
    "yl" '(avy-copy-line :which-key "avy copy line")
    "w" '(:ignore t :which-key "window")
    "wr" '(hydra-writing/body :which-key "hydra writing")
    "ww" '(other-window :which-key "other window")
    "wq" '(delete-window :which-key "delete window")
    "wk" '(delete-window :which-key "delete window")
    "wD" '(delete-other-windows :which-key "delete other windows")
    "wS" '(split-window-right :which-key "split window right")
    "w|" '(split-window-right :which-key "split window right")
    "ws" '(split-window-below :which-key "split window below")
    "w-" '(split-window-below :which-key "split window below")
    "c"  '(find-file user-emacs-directory :which-key "find emacs config files")
    "."  '(find-file :which-key "find file")
    ","  '(consult-buffer :which-key "consult buffer")
    ))

;; Hydra
(use-package hydra
  :ensure t)

(defhydra hydra-text-scale (:hint nil :color blue :timeout 6)
  "
^Text Scale^
-------------
_j_: in   %`text-scale-mode-amount  _r_: reset
_k_: out  %`text-scale-mode-amount  _q_: quit
"
  ("j" text-scale-increase)
  ("k" text-scale-decrease)
  ("r" (text-scale-set 0))
  ("q" nil))

;; Hydra for magit
(defhydra hydra-magit (:color blue :hint nil)
  "
^Magit:
^-----------------------------
^_s_: status  ^_c_: commit  ^_C_: checkout  
^_f_: fetch   ^_P_: push    ^_p_: pull
^_b_: blame   ^_B_: branch  ^_d_: diff
^_g_: magit   ^_l_: log     ^_q_: quit
"
  ("g" magit)
  ("s" magit-status)
  ("c" magit-commit)
  ("C" magit-checkout)
  ("P" magit-push)
  ("l" magit-log-current)
  ("f" magit-fetch)
  ("p" magit-pull)
  ("b" magit-blame-addition)
  ("B" magit-branch)
  ("d" magit-diff)
  ("q" nil :color red))

;; Hydra for dired
(defhydra hydra-dired (:hint nil :color pink)
  "
^Navigation^          ^Mark^               ^Actions^                ^Others^
----------------------------------------------------------------------------------
_j_: Next             _m_: Mark            _C_: Copy               _+_ : Create directory
_k_: Previous         _u_: Unmark          _R_: Rename/move        _._ : Refresh
_^_: Up directory     _U_: Unmark all      _D_: Delete             _q_: Quit hydra
_f_: Find file

"
  ("j" dired-next-line)
  ("k" dired-previous-line)
  ("^" dired-up-directory)
  ("f" dired-find-file)
  
  ("m" dired-mark)
  ("u" dired-unmark)
  ("U" dired-unmark-all-marks)
  
  ("C" dired-do-copy)
  ("R" dired-do-rename)
  ("D" dired-do-delete)
  
  ("+" dired-create-directory)
  ("." revert-buffer)

  ("q" nil :color blue))

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "s") 'hydra-dired/body)))

;; Hydra for evil-mc and evil-multiedit
(defhydra hydra-multi-cursors (:hint nil :color pink :timeout 6)
  "
^Evil-MC^             ^Evil-Multiedit^
----------------------^-------------------
^Cursors^             ^Regions^
_g_: Make & Go        ^_r_: Match symbol & next
_G_: Make & Stay      ^_R_: Match symbol & prev
_j_: Next Line        ^_m_: Match all
_k_: Prev Line        ^_M_: Unmark all
_u_: Undo last cursor ^_s_: Match & next
_U_: Undo all cursors ^_S_: Match & previous
^ ^                   ^_RET_: Restrict region
_q_: Quit mc          ^_Q_: Exit multiedit
"
  ;; evil-mc
  ("g" evil-mc-make-and-goto-next-match)
  ("G" evil-mc-make-cursor-here)
  ("j" evil-mc-make-cursor-move-next-line)
  ("k" evil-mc-make-cursor-move-prev-line)
  ("u" evil-mc-undo-last-added-cursor)
  ("U" evil-mc-undo-all-cursors)

  ;; evil-multiedit
  ("r" evil-multiedit-match-symbol-and-next)
  ("R" evil-multiedit-match-symbol-and-prev)
  ("m" evil-multiedit-match-all)
  ("M" evil-multiedit-unmark-all)
  ("s" evil-multiedit-match-and-next)
  ("S" evil-multiedit-match-and-prev)
  ("RET" evil-multiedit-toggle-or-restrict-region)
  ("Q" evil-multiedit-abort :color blue)

  ("q" nil :color blue))


;; Hydra for writing
(defhydra hydra-writing (:hint nil :exit t)
  "
  Writing Commands
  ^Structure^          ^Counts^              ^Modes^                ^Revisions^
  ^---------^----------^-------^-------------^-----^----------------^---------^
  _c_: New Chapter     _w_: Count Chapter    _d_: Distraction Free  _r_: Create Revision Entry
  _s_: Scene Break     _C_: Count Region     _f_: Flyspell          _n_: Add Revision Note
  _N_: New Note        _t_: Total Count      _o_: Olivetti Mode     _u_: Update Status
  ^ ^                  ^ ^                   ^ ^                    _W_: Update Word Count
  "
  ;; Structure
  ("c" my/insert-chapter)
  ("s" my/insert-scene-break)
  ("N" (lambda () 
         (interactive)
         (org-insert-heading)
         (insert "Note: ")))
  
  ;; Counts
  ("w" my/word-count-chapter)
  ("C" my/word-count-region)
  ("t" (lambda ()
         (interactive)
         (save-excursion
           (goto-char (point-min))
           (my/word-count-region (point-min) (point-max)))))
  
  ;; Modes
  ("d" writeroom-mode)
  ("f" flyspell-mode)
  ("o" olivetti-mode)
  
  ;; Export
  ("e" org-export-dispatch)
  ("p" (lambda () 
         (interactive)
         (org-latex-export-to-pdf)))
  ("x" (lambda ()
         (interactive)
         (org-docx-export-to-docx)))

  ;; Revisions
  ("r" my/create-chapter-revision-entry)
  ("n" my/add-revision-note)
  ("u" my/update-chapter-status)
  ("W" my/update-word-count)
  
  ;; Exit
  ("b" nil "back"))

;; Optional: Make the hydra head colors match their function
(setq hydra-writing/heads-property
      '((:foreground "pink")))  ; Customize color as desired

;; Custom Avy function that places the cursor after the search string
(defun silver_computing_machine/avy-goto-char ()
  "Read one or many consecutive chars with 'avy-goto-char-timer and jump after the search string."
  (interactive)
  (unless (eq (avy-goto-char-timer) t)
    (forward-char (length avy-text))))


(use-package undo-fu)
