;; starting up hide the splash screen

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)


;; indentation

(setq standard-indent 2)


;; use 2 spaces for tabs
(defun rock-tabs ()
  (interactive)
  (set-variable 'tab-width 4)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))


;; Highlights matching parenthesis
(show-paren-mode 1)


;; Highlight current line
(global-hl-line-mode 1)


;; Show line-number in the mode line
(global-linum-mode t)


;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)


;; disable backup files
(setq make-backup-files nil)


;;  time
(display-time-mode 1)


;; hide the toolbar, menubar and scroll bar
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Yes and no
(defalias 'yes-or-no-p 'y-or-n-p)


;; cleaning up the buffer
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

(setq-default show-trailing-whitespace t)

;; LANGUAGE HOOKS

;; javascript

(defun js-custom ()
  "js-mode-hook"
  (setq js-indent-level 4))

(add-hook 'js-mode-hook 'js-custom)


;; PACKAGES CONFIG

;; Wakatime config
(global-wakatime-mode)

;; Projectile
(projectile-mode)

;; helm projectile

(require 'helm-projectile)
(helm-projectile-on)

;; flyspell

(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checking" t)


;; for performance
(setq flyspell-issue-welcome-flag nil)


(add-hook          'c-mode-hook 'flyspell-prog-mode)
(add-hook     'python-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)


;; web-mode flyspell settings
(defun web-mode-flyspell-verify ()
  (let* ((f (get-text-property (- (point) 1) 'face))
	 rlt)
    (cond
     ;; Check the words with these font faces, possibly.
     ;; this *blacklist* will be tweaked in next condition
     ((not (memq f '(web-mode-html-attr-value-face
		     web-mode-html-tag-face
		     web-mode-html-attr-name-face
		     web-mode-constant-face
		     web-mode-doctype-face
		     web-mode-keyword-face
		     web-mode-comment-face ;; focus on get html label right
		     web-mode-function-name-face
		     web-mode-variable-name-face
		     web-mode-css-property-name-face
		     web-mode-css-selector-face
		     web-mode-css-color-face
		     web-mode-type-face
		     web-mode-block-control-face)))
      (setq rlt t))
     ;; check attribute value under certain conditions
     ((memq f '(web-mode-html-attr-value-face))
      (save-excursion
	(search-backward-regexp "=['\"]" (line-beginning-position) t)
	(backward-char)
	(setq rlt (string-match "^\\(value\\|class\\|ng[A-Za-z0-9-]*\\)$"
				(thing-at-point 'symbol)))))
     ;; finalize the blacklist
     (t
      (setq rlt nil)))
    rlt))
(put 'web-mode 'flyspell-mode-predicate 'web-mode-flyspell-verify)

;; don't mark double word error
(defvar flyspell-check-doublon t
  "Check double word when calling `flyspell-highlight-incorrect-region'.")
(make-variable-buffer-local 'flyspell-check-doublon)

(defadvice flyspell-highlight-incorrect-region (around flyspell-highlight-incorrect-region-hack activate)
  (if (or flyspell-check-doublon (not (eq 'doublon (ad-get-arg 2))))
      ad-do-it))

(defun web-mode-hook-setup ()
  (flyspell-mode 1)
  (setq flyspell-check-doublon nil))

(add-hook 'web-mode-hook 'web-mode-hook-setup)

;; JavaScript and React flyspell
(defun js-flyspell-verify ()
  (let* ((f (get-text-property (- (point) 1) 'face)))
    ;; *whitelist*
    ;; only words with following font face will be checked
    (memq f '(js2-function-call
	      js2-function-param
	      js2-object-property
	      font-lock-variable-name-face
	      font-lock-string-face
	      font-lock-function-name-face))))
(put 'js2-mode 'flyspell-mode-predicate 'js-flyspell-verify)
(put 'rjsx-mode 'flyspell-mode-predicate 'js-flyspell-verify)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)


;; multiple cursors
(require 'multiple-cursors)

(global-set-key (kbd "C-c m e") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)

;; Emacs customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (monokai-alt)))
 '(custom-safe-themes
   (quote
    ("eea01f540a0f3bc7c755410ea146943688c4e29bea74a29568635670ab22f9bc" default)))
 '(package-selected-packages
   (quote
    (multiple-cursors web-mode wakatime-mode solarized-theme scss-mode sass-mode org-journal org-bullets monokai-alt-theme markdown-mode json-mode helm-projectile git-gutter flycheck auto-complete)))
 '(wakatime-api-key (getenv "WAKATIME_API_KEY"))
 '(wakatime-cli-path "/usr/local/bin/wakatime")
 '(wakatime-python-bin nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 136 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
