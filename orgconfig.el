;; Org-mode Configurations
(require 'org)

(setq org-use-speed-commands t
        org-return-follows-link t
        org-hide-emphasis-markers t
        org-completion-use-ido t
        org-outline-path-complete-in-steps nil
        org-src-fontify-natively t   ;; Pretty code blocks
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-todo-keywords '((sequence "TODO(t)" "DOING(g)" "|" "DONE(d)")
                            (sequence "|" "CANCELED(c)")))

  (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
  (add-to-list 'auto-mode-alist '(".*/[0-9]*$" . org-mode))   ;; Journal entries

(font-lock-add-keywords            ; A bit silly but my headers are now
   'org-mode `(("^\\*+ \\(TODO\\) "  ; shorter, and that is nice canceled
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "⚑")
                          nil)))
               ("^\\*+ \\(DOING\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "⚐")
                          nil)))
               ("^\\*+ \\(CANCELED\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "✘")
                          nil)))
               ("^\\*+ \\(DONE\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "✔")
                          nil)))))


;; Org Habits

(require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

;; Journaling

(require 'org-journal)
(setq org-journal-dir "~/Org/journal/")
(setq org-journal-date-format "#+TITLE: Journal Entry- %e %b %Y (%A)")


;; Org-capture template
(global-set-key [f8] 'org-capture)
(setq org-capture-templates (quote (
            ("j" "Daily Journal" entry (file+olp+datetree "~/Org/journal/daily.org")
             "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
             :clock-in :clock-resume
             :empty-lines 1)
                    ("d" "Daily log" entry (file "~/Org/Tasks.org") "* %T %?")
)))


;; org agenda files

(setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
      ("Tasks.org" :maxlevel . 1)))

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-agenda-files
  '("~/Org/Tasks.org"
    "~/Org/Monthly.org"
    "~/Org/Projects.org"
    "~/Org/Habits.org"
    "~/Org/Birthdays.org"))

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)


;; asterisks to bullets
(require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

;; list asterisks to Unicode bullets
(font-lock-add-keywords 'org-mode
  '(("^ +\\([-*]\\) "
     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


;; for exporting

(require 'ox-html)
(setq org-html-postamble nil)
(setq org-export-with-section-numbers nil)
(setq org-export-with-toc nil)
(setq org-html-head-extra "
     <link href='http://fonts.googleapis.com/css?family=Source+Sans+Pro:400,700,400italic,700italic&subset=latin,latin-ext' rel='stylesheet' type='text/css'>
     <link href='http://fonts.googleapis.com/css?family=Source+Code+Pro:400,700' rel='stylesheet' type='text/css'>
     <style type='text/css'>
        body {
           font-family: 'Source Sans Pro', sans-serif;
        }
        pre, code {
           font-family: 'Source Code Pro', monospace;
        }
     </style>")



;; litrate programming


(add-to-list 'org-src-lang-modes '("dot" . "graphviz-dot"))

(org-babel-do-load-languages 'org-babel-load-languages
                               '((shell         . t)
                                 (js         . t)
                                 (emacs-lisp . t)
                                 (python     . t)
                                 (dot        . t)
                                 (css        . t)))

;; syntax hightlighting
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)


;; Estimating WPM
(require 'org-clock)
(defun my/org-entry-wpm ()
  (interactive)
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (let* ((words (count-words-region (point-min) (point-max)))
       (minutes (org-clock-sum-current-item))
       (wpm (/ words minutes)))
  (message "WPM: %d (words: %d, minutes: %d)" wpm words minutes)
  (kill-new (number-to-string wpm))))))
