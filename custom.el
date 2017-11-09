;; starting up hide the splash screen
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)


;; indentation

(setq standard-indent 2)


;; use 2 spaces for tabs
(defun die-tabs ()
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (monokai-alt)))
 '(custom-safe-themes
   (quote
    ("eea01f540a0f3bc7c755410ea146943688c4e29bea74a29568635670ab22f9bc" default)))
 '(wakatime-api-key (getenv "WAKATIME_API_KEY"))
 '(wakatime-cli-path "/usr/local/bin/wakatime")
 '(wakatime-python-bin nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 136 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
