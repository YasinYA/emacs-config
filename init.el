;; STARTING UP

(require 'package)

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; PERSONAL INFORMATION

(setq user-full-name "Yasin Yusuf"
            user-mail-address "yaasiinyuusuf78@gmail.com")

;; for loading files

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))


(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))


;; Update your local package index
(unless package-archive-contents
  (package-refresh-contents))

;; Increase the garbage collection threshold to 500 MB to ease startup
(setq gc-cons-threshold (* 500 1024 1024))


;; must have packages
(setq my/packages
  '(auto-complete
    git-gutter
    multiple-cursors
    projectile
    helm
    helm-projectile
    json-reformat
    flyspell
    tide
    company
    go-autocomplete
    add-node-modules-path
    flycheck
    doom-themes
    doom-modeline
    all-the-icons
    dumb-jump
    neotree
    treemacs

    ;; Modes
    wakatime-mode
    json-mode
    go-mode
    markdown-mode
    scss-mode
    sass-mode
    prettier-js
    js2-mode
    web-mode
    emmet-mode
    typescript-mode
    rjsx-mode

    ;; Themes
    monokai-alt-theme
    twilight-bright-theme

    ;;org packages
    org-journal
    org-bullets))

(setq byte-compile-warnings '(cl-functions))


;; and install them all if they are not already installed
(dolist (package my/packages)
  (unless (package-installed-p package)
    (message "Installing %s" package)
        (package-install package)))

;; keep customize settings in their own file
(setq custom-file
      (expand-file-name "custom.el"
			user-emacs-directory))
(when (file-exists-p custom-file)
    (load custom-file))


;; require org mode settings
(load-user-file "orgconfig.el")

;; Garbage collector - decrease threshold to 5 MB
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))
