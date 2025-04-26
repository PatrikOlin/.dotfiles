(use-package writeroom-mode)
;; (use-package olivetti)

;; Writing-specific visual settings
(setq org-hide-emphasis-markers t)  ; Hide markup symbols like *bold* /italic/
(setq org-pretty-entities t)        ; Show entities like \alpha as UTF-8 characters
(setq org-startup-indented t)       ; Enable org-indent-mode by default
(setq org-startup-folded t)         ; Start files with all sections folded

;; Line wrapping and visual settings for prose
(visual-line-mode 1)
(visual-fill-column-mode 1)
(setq visual-fill-column-width 90)
(setq-default fill-column 90)

;; Custom functions for word count
(defun my/word-count-region (start end)
  "Count the number of words in the selected region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (let ((count 0))
        (while (forward-word 1)
          (setq count (1+ count)))
        (message "Word count: %d" count)))))

(defun my/word-count-chapter ()
  "Count words in current chapter (heading)."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let ((start (point))
          (end (org-end-of-subtree)))
      (my/word-count-region start end))))

;; Key bindings for writing functions
(global-set-key (kbd "C-c w") 'my/word-count-chapter)

;; Auto-save settings
(auto-save-visited-mode 1)
(setq auto-save-timeout 20)  ; Save after 20 seconds of idle time

;; Backup settings - keep backups in a separate directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

;; Custom TODO states for tracking writing progress
(setq org-todo-keywords
      '((sequence "DRAFT(d)" "REVISING(r)" "FEEDBACK(f)" "|" "COMPLETE(c)")))

;; Custom org-mode link types for cross-referencing
(defun org-link-set-parameters ()
 "character"
 :follow (lambda (name) (org-open-link-from-string (format "[[*%s]]" name)))
 :export (lambda (name desc format)
           (or desc name)))

;; Functions for inserting common writing elements
(defun my/insert-scene-break ()
  "Insert a scene break marker."
  (interactive)
  (insert "\n\n* * *\n\n"))

(defun my/insert-chapter ()
  "Insert a new chapter template."
  (interactive)
  (insert "** Chapter \n\n"))

;; Custom key bindings for writing functions
(global-set-key (kbd "C-c s") 'my/insert-scene-break)
(global-set-key (kbd "C-c c") 'my/insert-chapter)

;; Flyspell configuration
(add-hook 'org-mode-hook 'flyspell-mode)
(setq ispell-program-name "hunspell")
(setq ispell-dictionary "en_GB")
(setq ispell-local-dictionary-alist
      '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8)))

;; Optional: Enable writeroom-mode for distraction-free writing
;; Note: Requires the writeroom-mode package to be installed
(when (require 'writeroom-mode nil t)
  (setq writeroom-width 90)
  (setq writeroom-mode-line t)
  (setq writeroom-bottom-divider-width 0))

;; Optional: Enable olivetti-mode for centered text
;; Note: Requires the olivetti package to be installed
(when (require 'olivetti nil t)
  (setq olivetti-body-width 90))


;; Custom functions for managing revisions in a separate file

(defvar my/writing-main-file "~/silver_computing_machine/main.org"
  "Path to main writing file.")

(defvar my/writing-revision-file "~/silver_computing_machine/revisions.org"
  "Path to revision tracking file.")

(defun my/ensure-revision-file ()
  "Ensure revision file exists with proper structure."
  (unless (file-exists-p my/writing-revision-file)
    (with-temp-file my/writing-revision-file
      (insert "#+TITLE: Revision Tracking\n\n"
              "* Active Draft Chapters\n\n"
              "* Revision Priorities\n"
              "** High Priority\n"
              "** Medium Priority\n"
              "** Low Priority\n"))))

(defun my/get-chapter-title ()
  "Get the title of the current chapter."
  (save-excursion
    (org-back-to-heading t)
    (let ((title (org-get-heading t t t t)))
      (if (string-match "Chapter \\(.*\\)" title)
          title
        (error "Not in a chapter heading")))))

(defun my/create-chapter-revision-entry ()
  "Create a new revision entry for the current chapter."
  (interactive)
  (my/ensure-revision-file)
  (let ((chapter-title (my/get-chapter-title)))
    (find-file my/writing-revision-file)
    (goto-char (point-min))
    (re-search-forward "^\\* Active Draft Chapters")
    (org-end-of-line)
    (newline 2)
    (insert (format "** %s\n" chapter-title)
            "*** Current Status: First Draft\n"
            "*** Word Count: \n\n"
            "**** Quick Revision Notes\n"
            "- [ ] \n\n"
            "**** Future Connection Points\n"
            "- [ ] \n\n"
            "**** Consistency Tracking\n"
            "***** AI Behaviors & Characteristics\n"
            "- \n\n"
            "***** Theme Development\n"
            "- \n\n"
            "**** Cross-Chapter Elements\n"
            "***** Characters\n"
            "- \n\n"
            "***** World Details\n"
            "- \n\n"
            "***** Theme Progress\n"
            "- \n\n")))

(defun my/add-revision-note ()
  "Add a quick revision note for the current chapter."
  (interactive)
  (let ((chapter-title (my/get-chapter-title))
        (note (read-string "Revision note: ")))
    (find-file my/writing-revision-file)
    (goto-char (point-min))
    (if (re-search-forward (format "^\\*\\* %s" chapter-title) nil t)
        (progn
          (re-search-forward "^\\*\\*\\*\\* Quick Revision Notes")
          (org-end-of-subtree)
          (newline)
          (insert (format "- [ ] %s" note)))
      (user-error "No revision entry found for %s. Create one first" chapter-title))))

(defun my/update-chapter-status ()
  "Update the status of the current chapter."
  (interactive)
  (let* ((chapter-title (my/get-chapter-title))
         (status (completing-read "New status: "
                                '("First Draft"
                                  "Revision Phase"
                                  "Ready for Review"
                                  "Final Draft"))))
    (find-file my/writing-revision-file)
    (goto-char (point-min))
    (when (re-search-forward (format "^\\*\\* %s" chapter-title) nil t)
      (re-search-forward "^\\*\\*\\* Current Status: .*$")
      (replace-match (format "*** Current Status: %s" status)))))

(defun my/update-word-count ()
  "Update the word count for the current chapter."
  (interactive)
  (save-excursion
    (let* ((chapter-title (my/get-chapter-title))
           (start (progn (org-back-to-heading) (point)))
           (end (org-end-of-subtree))
           (words (count-words start end)))
      (find-file my/writing-revision-file)
      (goto-char (point-min))
      (when (re-search-forward (format "^\\*\\* %s" chapter-title) nil t)
        (re-search-forward "^\\*\\*\\* Word Count: .*$")
        (replace-match (format "*** Word Count: %d" words))))))

;; First, clear everything
(setq org-latex-classes nil)

(add-to-list 'org-latex-classes
             '("book"
               "\\documentclass[12pt]{book}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{palatino}
\\usepackage[a4paper,inner=4cm,outer=3cm,top=2.5cm,bottom=2.5cm]{geometry}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{capt-of}
\\usepackage{parskip}
\\usepackage{titlesec}
\\usepackage{hyperref}

% Preserve line breaks and set spacing
\\setlength{\\parindent}{0em}
\\setlength{\\parskip}{1em}
\\renewcommand{\\baselinestretch}{1.1}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")  
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")))

;; Set how org levels map to LaTeX
(setq org-export-headline-levels 4)

;; Function to handle chapter conversion
(defun my/org-latex-format-headline-function (todo todo-type priority text tags info)
  "Format headline with proper chapter handling."
  (when (and tags (member "ignore" tags))
    (setq text ""))
  text)

(setq org-latex-format-headline-function 'my/org-latex-format-headline-function)

;; Basic settings
(setq org-latex-default-class "book")
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)
(setq org-export-with-author t)
(setq org-export-with-title t)
(setq org-export-with-tags nil)
(setq org-export-preserve-breaks t)
