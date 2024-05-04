;;; neat-ui.el --- Neatmacs UI Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Packages:
;; - doom-themes
;; - doom-modeline
;; - helpful
;; - elisp-demos

;;; Code:
(require 'use-package)

;;; Sane defaults
(customize-set-variable 'inhibit-startup-message t)
(customize-set-variable 'visible-bell t)

(scroll-bar-mode -1)			; Disable visible scrollbar
(tool-bar-mode -1)			; Disable the toolbar
(menu-bar-mode -1)			; Disable te menu bar
(tooltip-mode -1)			; Disable tooltips
(set-fringe-mode 10)			; Give some breathing room

(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Make ESC quit prompts

;;; Font
(defgroup neatmacs-ui '()
  "User interface related configuration for Neatmacs."
  :tag "Neat UI"
  :group 'neatmacs)

(defcustom neatmacs-ui-circadian nil
  "When non-nil, will use light and dark themes depending on the sun."
  :group 'neatmacs-ui
  :type 'boolean)

(defcustom neatmacs-ui-light-theme 'modus-operandi-deuteranopia
  "Light theme for Neatmacs."
  :group 'neatmacs-ui
  :type 'symbol)

(defcustom neatmacs-ui-dark-theme 'modus-vivendi-deuteranopia
  "Dark theme for Neatmacs."
  :group 'neatmacs-ui
  :type 'symbol)


;;; Packages:

(use-package doom-modeline
  :custom
  (doom-modeline-height 15)
  :init
  (doom-modeline-mode 1))

(use-package modus-themes
  ;; :straight (:type built-in
  ;;                  :pre (emacs-version <= "28.0")) ; Built-in from 28.0
  :custom
  ;; Use bold and italic text in more code constructs
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)

  (modus-themes-variable-pitch-ui t)
  (modus-themes-mixed-fonts t)

  ;; Org mode block style
  (modus-themes-org-blocks 'gray-background)

  ;; Toggle between color blind themes
  (modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia))

  (modus-themes-common-palette-overrides
   '(
     ;; Disable modeline border
     (border-mode-line-active unspecified)
     (border-mode-line-inactive unspecified)

     ;;;(fringe unspecified)))
     ))

  :init
  (load-theme 'modus-vivendi-deuteranopia t))


(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)

  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  ("C-c C-d" . helpful-at-point)

  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  ("C-h F" . helpful-function)

  ;; By default, C-h C is bound to describe `describe-coding-system'. I
  ;; don't find this very useful, but it's frequently useful to only
  ;; look at interactive functions.
  ("C-h C" . helpful-command)
  :commands (helpful-callable helpful-function helpful-variable helpful-at-point helpful-command helpful-key)
  :custom (helpful-max-buffers 10))


;;; Elisp-demos
;; Provides Elisp API demos in help buffers
(use-package elisp-demos
  :after helpful
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; TODO: Consider removing
;; Seems like transient is a more used alternative
;; (use-package hydra)

;; TODO: Consider removing
;; (use-package biome
;;   :straight (:host github :repo "SqrtMinusOne/biome"))


(provide 'neat-ui)
;;; neat-ui.el ends here
