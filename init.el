;;; init.el -*- lexical-binding: t; -*-
;;; Bootstrapping
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-ensure t)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Neatmacs loaded in %s."
                     (emacs-init-time))))

;;; Set default coding system
(set-default-coding-systems 'utf-8)
;;; Warn about opening files more than 100MB in size
(customize-set-variable 'large-file-warning-threshold (* 100 1000 1000))

(defgroup neatmacs '()
  "A Neat Emacs Configuration"
  :tag "Neatmacs"
  :group 'emacs)

(defvar neatmacs-config-file (expand-file-name "config.el" user-emacs-directory)
  "User's Neatmacs configuration file.")

(defvar neatmacs-load-custom-file t
  "Whether to load `custom.el` or not. When non-nil, `custom.el` is loaded after `config.el`")

(setq package-enable-at-startup nil)

;;; Add Modules to load path
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/applications" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/development" user-emacs-directory))

;;; Load user configuration
(when (file-exists-p neatmacs-config-file)
  (load neatmacs-config-file nil 'nomessage))

;;; Use a customization file to store values instead of init.el
(customize-set-variable 'custom-file
                        (expand-file-name "custom.el" user-emacs-directory))

(when neatmacs-load-custom-file
  (load custom-file t))

;;; Decrease garbage collection threshold
(setq gc-cons-threshold (* 80 1000 1000))
