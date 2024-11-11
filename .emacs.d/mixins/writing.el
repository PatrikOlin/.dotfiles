(use-package org-novelist
  :ensure nil
  :load-path "~/.dotfiles/.emacs.d/"  ; The directory containing 'org-novelist.el'
  :custom
    (org-novelist-author "Patrik Olin")  ; The default author name to use when exporting a story. Each story can also override this setting
    (org-novelist-author-email "patrik@olin.dev")  ; The default author contact email to use when exporting a story. Each story can also override this setting
    (org-novelist-automatic-referencing-p nil))  ; Set this variable to 't' if you want Org Novelist to always keep note links up to date. This may slow down some systems when operating on complex stories. It defaults to 'nil' when not set
