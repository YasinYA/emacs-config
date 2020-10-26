;; starting up hide the splash screen
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; Projectile dirs
(setq projectile-project-search-path '("~/Projects/" "~/Work/"))

;; Emojis
(use-package emojify
  :hook (after-init . global-emojify-mode))
(add-hook 'after-init-hook #'global-emojify-mode)

;; Company Global mode
(add-hook 'after-init-hook 'global-company-mode)

;; Dumb-jump
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;; GO to
(dumb-jump-mode)

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


;; Although i didn't use them, but i still think it may
;; useful to keep and store all backup and autosave files in the directory
(setq backup-directory-alist
      `((".*" . ,"~/.emacs.d/emacs_backup")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/auto_save" t)))

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
  ;; Remove tmux artifacts from region.
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
;; This doesn't work anymore.
;;(projectile-mode)

;; You need to explictly set the projectile base command
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; helm projectile

(require 'helm-projectile)
(helm-projectile-on)


(require 'flycheck)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))

;; flyspell

(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checking" t)


;; for performance
(setq flyspell-issue-welcome-flag nil)


(add-hook          'c-mode-hook 'flyspell-prog-mode)
(add-hook     'python-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)


;; Emmet
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(global-set-key (kbd "C-`") 'emmet-expand-line)


;; use project node_modules for eslint
(add-hook 'flycheck-mode-hook 'add-node-modules-path)

;; don't mark double word error
(defvar flyspell-check-doublon t
  "Check double word when calling `flyspell-highlight-incorrect-region'.")
(make-variable-buffer-local 'flyspell-check-doublon)

(defadvice flyspell-highlight-incorrect-region (around flyspell-highlight-incorrect-region-hack activate)
  (if (or flyspell-check-doublon (not (eq 'doublon (ad-get-arg 2))))
      ad-do-it))


;; JavaScript, Typescript and React

;; Tide setup
(add-to-list 'exec-path "/usr/bin/node")

(defun setup-tide-mode ()
  (interactive)
  ;;  (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))
  (tide-setup)
  (if (file-exists-p (concat tide-project-root "node_modules/typescript/bin/tsserver"))
    (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver"))
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (setq tide-format-options '(:indentSize 2 :tabSize 2 :insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
  (local-set-key (kbd "C-c d") 'tide-documentation-at-point)
  (company-mode +1)
  (setq company-minimum-prefix-length 1))

(require 'use-package)
(use-package tide
  :ensure t
  :config
  (progn
    (company-mode +1)
    ;; aligns annotation to the right hand side
    (setq company-tooltip-align-annotations t)
    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  ))

;; use web-mode + tide-mode for javascript instead
(use-package js2-mode
  :ensure t
  :config
  (progn
    (add-hook 'js2-mode-hook #'setup-tide-mode)
    ;; configure javascript-tide checker to run after your default javascript checker
    (setq js2-basic-offset 2)
    (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
    (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))))

;; (add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(use-package json-mode
  :ensure t
  :config
  (progn
    (flycheck-add-mode 'json-jsonlint 'json-mode)
    (add-hook 'json-mode-hook 'flycheck-mode)
    (setq js-indent-level 2)
    (add-to-list 'auto-mode-alist '("\\.json" . json-mode))))

(use-package web-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js"     . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html"   . web-mode))
    ;; this magic incantation fixes highlighting of jsx syntax in .js files
    (setq web-mode-content-types-alist
          '(("jsx" . "\\.js[x]?\\'")))
    (add-hook 'web-mode-hook
              (lambda ()
                (setq web-mode-code-indent-offset 2)
                (when (string-equal "tsx" (file-name-extension buffer-file-name))
                  (setup-tide-mode))
                (when (string-equal "jsx" (file-name-extension buffer-file-name))
                  (setup-tide-mode))
                (when (string-equal "js" (file-name-extension buffer-file-name))
                  (progn
                    (setup-tide-mode)
                    (with-eval-after-load 'flycheck
                      (flycheck-add-mode 'typescript-tslint 'web-mode)
                      (flycheck-add-mode 'javascript-tide 'web-mode))))))
    ))
    ;; enable typescript-tslint checker


;; flyspell
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(ansi-term-color-vector
   [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"] t)
 '(custom-enabled-themes '(doom-one))
 '(custom-safe-themes
   '("2d1fe7c9007a5b76cea4395b0fc664d0c1cfd34bb4f1860300347cdad67fb2f9" default))
 '(doom-modeline-env-enable-python t)
 '(doom-modeline-env-python-executable "python3.8")
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#d9d9d9")
 '(flycheck-python-pycompile-executable "python3.8")
 '(package-selected-packages
   '(treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil use-package emmet-mode doom-themes doom-modeline go-autocomplete exec-path-from-shell go-mode prettier-js add-node-modules-path twilight-bright-theme multiple-cursors wakatime-mode solarized-theme scss-mode sass-mode org-journal org-bullets monokai-alt-theme markdown-mode json-mode helm-projectile git-gutter flycheck auto-complete))
 '(wakatime-api-key (getenv "WAKATIME_API_KEY"))
 '(wakatime-cli-path "/usr/local/bin/wakatime")
 '(wakatime-python-bin nil)
 '(xterm-mouse-mode t))

;; Prettier
(require 'prettier-js)
(add-hook 'js-mode-hook 'prettier-js-mode)

(setq prettier-js-args '(
  "--trailing-comma" "none"
  "--bracket-spacing" "true"
  "--single-quote" "true"
  "--no-semi" "true"
  "--jsx-single-quote" "true"
  "--jsx-bracket-same-line" "true"
  "--tab-width" "4"))


;; multiple cursors
(require 'multiple-cursors)

(global-set-key (kbd "C-c m e") 'mc/edit-lines)
(global-set-key (kbd "C-c >") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c <") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)

;; Go

;; Godoc
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;; GOPATH
(setenv "GOPATH" "/home/yasinya/go")

;; Gofmt Auto call on save
(add-to-list 'exec-path "/home/yasinya/go/bin/")
(add-hook 'before-save-hook 'gofmt-before-save)

;; Go Autocomplete
(defun auto-complete-for-go ()
(auto-complete-mode 1))
 (add-hook 'go-mode-hook 'auto-complete-for-go)

 (with-eval-after-load 'go-mode
   (require 'go-autocomplete))


;; Goimports
(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
)
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; All the icons
(require 'all-the-icons)
(all-the-icons-insert-icons-for 'material)

;; Doom Modeline
(require 'doom-modeline)
(doom-modeline-mode 1)

;; Emacs customize


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 136 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))



;; Doom Modeline Customizations

;; How tall the mode-line should be. It's only respected in GUI.
;; If the actual char height is larger, it respects the actual height.
(setq doom-modeline-height 25)

;; How wide the mode-line bar should be. It's only respected in GUI.
(setq doom-modeline-bar-width 3)

;; How to detect the project root.
;; The default priority of detection is `ffip' > `projectile' > `project'.
;; nil means to use `default-directory'.
;; The project management packages have some issues on detecting project root.
;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
;; to hanle sub-projects.
;; You can specify one if you encounter the issue.
(setq doom-modeline-project-detection 'project)

;; Determines the style used by `doom-modeline-buffer-file-name'.
;;
;; Given ~/Projects/FOSS/emacs/lisp/comint.el
;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
;;   truncate-with-project => emacs/l/comint.el
;;   truncate-except-project => ~/P/F/emacs/l/comint.el
;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
;;   truncate-all => ~/P/F/e/l/comint.el
;;   relative-from-project => emacs/lisp/comint.el
;;   relative-to-project => lisp/comint.el
;;   file-name => comint.el
;;   buffer-name => comint.el<2> (uniquify buffer name)
;;
;; If you are experiencing the laggy issue, especially while editing remote files
;; with tramp, please try `file-name' style.
;; Please refer to https://github.com/bbatsov/projectile/issues/657.
(setq doom-modeline-buffer-file-name-style 'truncate-upto-project)

;; Whether display icons in mode-line. Respects `all-the-icons-color-icons'.
;; While using the server mode in GUI, should set the value explicitly.
(setq doom-modeline-icon (display-graphic-p))

;; Whether display the icon for `major-mode'. Respects `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)

;; Whether display the colorful icon for `major-mode'.
;; Respects `doom-modeline-major-mode-icon'.
(setq doom-modeline-major-mode-color-icon t)

;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
(setq doom-modeline-buffer-state-icon t)

;; Whether display the modification icon for the buffer.
;; Respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
(setq doom-modeline-buffer-modification-icon t)

;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
(setq doom-modeline-unicode-fallback nil)

;; Whether display the minor modes in mode-line.
(setq doom-modeline-minor-modes (featurep 'minions))

;; If non-nil, a word count will be added to the selection-info modeline segment.
(setq doom-modeline-enable-word-count nil)

;; Major modes in which to display word count continuously.
;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
(setq doom-modeline-continuous-word-count-modes '(text-mode))

;; Whether display the buffer encoding.
(setq doom-modeline-buffer-encoding t)

;; Whether display the indentation information.
(setq doom-modeline-indent-info nil)

;; If non-nil, only display one number for checker information if applicable.
(setq doom-modeline-checker-simple-format t)

;; The maximum number displayed for notifications.
(setq doom-modeline-number-limit 99)

;; The maximum displayed length of the branch name of version control.
(setq doom-modeline-vcs-max-length 12)

;; Whether display the perspective name. Non-nil to display in mode-line.
(setq doom-modeline-persp-name t)

;; If non nil the default perspective name is displayed in the mode-line.
(setq doom-modeline-display-default-persp-name nil)

;; Whether display the `lsp' state. Non-nil to display in mode-line.
(setq doom-modeline-lsp t)

;; Whether display the GitHub notifications. It requires `ghub' package.
(setq doom-modeline-github nil)

;; The interval of checking GitHub.
(setq doom-modeline-github-interval (* 30 60))

;; Whether display the modal state icon.
;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
(setq doom-modeline-modal-icon t)

;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
(setq doom-modeline-mu4e t)

;; Whether display the IRC notifications. It requires `circe' or `erc' package.
(setq doom-modeline-irc t)

;; Function to stylize the irc buffer names.
(setq doom-modeline-irc-stylize 'identity)

;; Whether display the environment version.
(setq doom-modeline-env-version t)
;; Or for individual languages
(setq doom-modeline-env-enable-python t)
(setq doom-modeline-env-enable-ruby t)
(setq doom-modeline-env-enable-perl t)
(setq doom-modeline-env-enable-go t)
(setq doom-modeline-env-enable-elixir t)
(setq doom-modeline-env-enable-rust t)

;; Change the executables to use for the language version string
(setq doom-modeline-env-python-executable "python3.8") ; or `python-shell-interpreter'
(setq doom-modeline-env-ruby-executable "ruby")
(setq doom-modeline-env-perl-executable "perl")
(setq doom-modeline-env-go-executable "go")
(setq doom-modeline-env-elixir-executable "iex")
(setq doom-modeline-env-rust-executable "rustc")

;; What to dispaly as the version while a new one is being loaded
(setq doom-modeline-env-load-string "...")

;; Hooks that run before/after the modeline version string is updated
(setq doom-modeline-before-update-env-hook nil)
(setq doom-modeline-after-update-env-hook nil)


;; Keybindings

;; CTRL + SHIFT + d for duplicating a line
(defun duplicate-line ()
   (interactive)
   (save-mark-and-excursion
     (beginning-of-line)
     (insert (thing-at-point 'line t))))

 (global-set-key (kbd "C-S-d") 'duplicate-line)

 ;; CTRL + SHIFT + J , K for moving line up and down

 (defun move-line-down ()
   (interactive)
   (let ((col (current-column)))
     (save-excursion
       (forward-line)
       (transpose-lines 1))
     (forward-line)
     (move-to-column col)))

 (defun move-line-up ()
   (interactive)
   (let ((col (current-column)))
     (save-excursion
       (forward-line)
       (transpose-lines -1))
     (forward-line -1)
     (move-to-column col)))

 (global-set-key (kbd "C-S-j") 'move-line-down)
 (global-set-key (kbd "C-S-k") 'move-line-up)


;; For project tree
(require 'treemacs)
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ; (treemacs-resize-icons 32)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("C-x t t"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("M-0"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-persp ;;treemacs-persective if you use perspective.el vs. persp-mode
  :after treemacs persp-mode ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(treemacs-git-mode 'extended)
(with-eval-after-load 'treemacs
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))
