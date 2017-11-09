;; STARTING UP

(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; PERSONAL INFORMATION

(setq user-full-name "Yasin Yusuf"
            user-mail-address "yaasiinyuusuf78@gmail.com")


;; Update your local package index
(unless package-archive-contents
  (package-refresh-contents))


;; must have packages
(setq my/packages
  '(auto-complete
    git-gutter
    projectile
    helm
    json-reformat

    ;; Modes
    wakatime-mode
    json-mode
    markdown-mode
    scss-mode
    sass-mode))

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

