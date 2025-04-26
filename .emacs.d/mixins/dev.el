(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package emacs
  :config
  ;; Treesitter config

  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)
          (go-mode . go-ts-mode)
          (go-mod-mode . go-mod-ts-mode)
          (elixir-mode . elixir-ts-mode)
          (rust-mode . rust-ts-mode)
          (toml-mode . toml-ts-mode)
          (gdscript-mode . gdscript-ts-mode)
          (lua-mode . lua-ts-mode)))

  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))

  ;; Tree sitter language grammars
  (setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (astro "https://github.com/virchau13/tree-sitter-astro")
     (heex "https://github.com/phoenixframework/tree-sitter-heex")
     (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (gdscript "https://github.com/PrestonKnopp/tree-sitter-gdscript")
     (toml "https://github.com/ikatyang/tree-sitter-toml")
     (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :bind (("s-g" . magit-status)
         ("C-c g" . magit-status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

;; Emacs ships with a lot of popular programming language modes. If it's not
;; built in, you're almost certain to find a mode for the language you're
;; looking for with a quick Internet search.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  ;; no :ensure t here because it's built-in
  ;; Configure hooks to automatically turn-on eglot for selected modes
  :hook
  (((python-mode ruby-mode elixir-mode js2-mode typescript-mode go-mode go-ts-mode css-mode) . eglot))
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-connect-timeout 30)
  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  (setq eglot-events-buffer-size 1000000)
  (setq eglot-autoshutdown t)

  ;; Enable all server capabilities
  (setq eglot-stay-out-of '())
  (setq eglot-ignored-server-capabilities '())

  (add-to-list 'eglot-server-programs '((elixir-mode elixir-ts-mode) . ("/home/olin/code/elixir-ls/release/language_server.sh")))
  (add-to-list 'eglot-server-programs '(rust-mode . ("/home/olin/.cargo/bin/rust-analyzer")))
  (add-to-list 'eglot-server-programs '((go-mode go-ts-mode) . ("/home/olin/.asdf/shims/gopls")))

  ;; Configure gopls explicitly
  (setq-default eglot-workspace-configuration
                '((:gopls . ((staticcheck . t)
                             (buildFlags . ["-tags=integration"])
                             (matcher . "CaseSensitive")
                             (analyses . ((unusedparams . t)
                                          (shadow . t)
                                          (unusedwrite . t)
                                          (unusedvariable . t)))))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot booster
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package eglot-booster
;;   :after eglot
;;   :config (eglot-booster-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Projectile
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :ensure t
  :init
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Web-mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.css\\'" "\\.js\\'" "\\.jsx\\'" "\\.svelte\\'")
  :config
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-quoting t)
  (add-hook 'web-mode-hook
            (lambda ()
              (setq indent-region-function 'web-mode-buffer-indent))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Auto switch modes
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Automatically set typescript-mode and start eglot for .ts files
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-hook 'typescript-ts-mode 'eglot-ensure)

(defun setup-svelte-mode ()
  "Hooks for Svelte mode."
  (setq web-mode-enable-auto-indentation nil))

(add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "svelte" (file-name-extension buffer-file-name))
              (setup-svelte-mode))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Copilot - Need to install from GitHub
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Clone the repository manually first:
;; git clone https://github.com/zerolfx/copilot.el ~/.emacs.d/copilot

;; Then add this to load copilot.el from the local directory:
;;(add-to-list 'load-path "~/.emacs.d/copilot")
;;(when (file-directory-p "~/.emacs.d/copilot")
;;  (require 'copilot nil t)
;;  (add-hook 'prog-mode-hook 'copilot-mode)
;;  (define-key copilot-mode-map (kbd "C-f") #'copilot-complete-or-accept)
;;  (define-key copilot-completion-map (kbd "<right>") #'copilot-accept-completion)
;;  (define-key copilot-completion-map (kbd "C-g") #'copilot-clear-overlay)
;;  (define-key copilot-completion-map (kbd "C-S-f") #'copilot-next-completion)
;;  (define-key copilot-completion-map (kbd "C-S-g") #'copilot-previous-completion))

;;(defun copilot-complete-or-accept ()
;;  "Command that either triggers a completion or accepts one if one
;;is available."
;;  (interactive)
;;  (if (copilot--overlay-visible)
;;      (progn
;;        (copilot-accept-completion))
;;    (copilot-complete)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Wakatime mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package wakatime-mode
  :ensure t
  :init
  (global-wakatime-mode))

(setq wakatime-api-key "06fe98b5-d106-4309-a710-d08e1d8d8215")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Magit
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver_computing_machine/magit-display-buffer (buffer)
  "Display the Magit buffer in the current window."
  (display-buffer buffer '(display-buffer-same-window)))

(setq magit-display-buffer-function #'silver_computing_machine/magit-display-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Gdscript mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package gdscript-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.gd\\'" . gdscript-mode))
  (add-to-list 'auto-mode-alist '("\\.tscn\\'" . gdscript-mode)))

(setq gdscript-godot-executable "/run/media/olin/Dundret/Godot/Godot_v4.4.1-stable_linux.x86_64")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Elixir ts mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elixir-ts-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.eex\\'" . elixir-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.heex\\'" . elixir-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.leex\\'" . elixir-ts-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Utility functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-mock-uuid ()
  "Generate a string that looks like a UUID (format: 8-4-4-4-12 hexadecimal digits)."
  (let ((hex-chars "0123456789abcdef"))
    (concat
     ;; 8 hex digits
     (mapconcat (lambda (_) (string (aref hex-chars (random 16)))) (make-list 8 nil) "")
     "-"
     ;; 4 hex digits
     (mapconcat (lambda (_) (string (aref hex-chars (random 16)))) (make-list 4 nil) "")
     "-"
     ;; 4 hex digits
     (mapconcat (lambda (_) (string (aref hex-chars (random 16)))) (make-list 4 nil) "")
     "-"
     ;; 4 hex digits
     (mapconcat (lambda (_) (string (aref hex-chars (random 16)))) (make-list 4 nil) "")
     "-"
     ;; 12 hex digits
     (mapconcat (lambda (_) (string (aref hex-chars (random 16)))) (make-list 12 nil) ""))))

(defun insert-mock-uuid ()
  "Insert a mock UUID at point."
  (interactive)
  (insert (generate-mock-uuid)))
