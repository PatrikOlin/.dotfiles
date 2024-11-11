;;; Silver computing machine
;;;
;;; Mixin: Development tools

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
;;;
;;; It is **STRONGLY** recommended that you use the base.el mixin if you want to
;;; use Eglot. Lots of completion things will work better.
;;;
;;; This will try to use tree-sitter modes for many languages. Please run
;;;
;;;   M-x treesit-install-language-grammar
;;;
;;; Before trying to use a treesit mode.

;;; Contents:
;;;
;;;  - Built-in config for developers
;;;  - Version Control
;;;  - Common file types
;;;  - Eglot, the built-in LSP client for Emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in config for developers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (((python-mode ruby-mode elixir-mode js2-mode typescript-mode go-mode css-mode) . eglot))

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-connect-timeout 30)

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  (setq eglot-events-buffer-size 1000000)
  (setq eglot-autoshutdown t)
  (add-to-list 'eglot-server-programs
               '(elixir-mode . ("/home/olin/code/elixir-ls/release/language_server.sh"))
               '(rust-mode . ("/home/olin/.cargo/bin/rust-analyzer")))
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
;;;   Copilot
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook (prog-mode . copilot-mode)
  :bind (("C-f" . 'copilot-complete-or-accept)
         :map copilot-completion-map
         ("<right>" . 'copilot-accept-completion)
         ("C-g" . 'copilot-clear-overlay)
         ("C-S-f" . 'copilot-next-completion)
         ("C-S-g" . 'copilot-previous-completion)))
 
(defun copilot-complete-or-accept ()
  "Command that either triggers a completion or accepts one if one
is available."
  (interactive)
  (if (copilot--overlay-visible)
      (progn
        (copilot-accept-completion))
    (copilot-complete)))


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

(setq gdscript-godot-executable "/run/media/olin/Dundret/Godot/Godot_v4.3-stable_linux.x86_64")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Rust ts mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (racer-mode)
              (cargo-minor-mode))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Elixir ts mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elixir-ts-mode
  :ensure t
  :init  
  (add-to-list 'auto-mode-alist '("\\.ex") 'elixir-mode)
  (add-to-list 'auto-mode-alist '("\\.exs") 'elixir-mode)
  (add-to-list 'auto-mode-alist '("\\.eex") 'elixir-mode)
  (add-to-list 'auto-mode-alist '("\\.heex") 'elixir-mode)
  (add-to-list 'auto-mode-alist '("\\.leex") 'elixir-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Dart and Flutter support
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dart mode
(use-package dart-mode
  :ensure t
  :hook (dart-mode . eglot-ensure)
  :config
  (setq dart-format-on-save t)
  (add-to-list 'eglot-server-programs
               '(dart-mode . ("dart" "language-server" "--protocol=lsp"))))

;; Flutter mode
(use-package flutter
  :ensure t
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path (string-trim (shell-command-to-string "asdf where flutter"))))

;; Treesitter Dart mode
(use-package dart-ts-mode
  :straight (:host github :repo "50ways2sayhard/dart-ts-mode")
  :ensure t)

;; Add Dart to treesit-language-source-alist
(add-to-list 'treesit-language-source-alist
             '(dart "https://github.com/UserNobody14/tree-sitter-dart"))

;; Remap dart-mode to dart-ts-mode when available
(when (treesit-ready-p 'dart)
  (add-to-list 'major-mode-remap-alist
               '(dart-mode . dart-ts-mode)))

;; Configure LSP for Dart/Flutter
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((dart-mode dart-ts-mode) . ("dart" "language-server" "--protocol=lsp"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Gptel
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package gptel
;;   :config)

(setq gptel-model "claude-3-5-sonnet-20240620"
 gptel-backend (gptel-make-anthropic "Claude"
               :stream t :key (my/get-secret "api-keys/claude")))

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
