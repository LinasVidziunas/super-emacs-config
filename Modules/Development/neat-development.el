;;; neat-development.el --- Neatmacs Development Configuration --- -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; - pretty-mode - prettify symbols
;; - hl-todo - highlight TODOs and other keywords in comments and strings
;; - magit-todos - display keyword entries from source code comments and Org files in the Magit status buffer.
;; - eglot - client for Language Server Protocol servers
;; - dap-mode - Debug Adapter Protocol
;; - magit - Git porcelain inside Emacs
;; - flycheck - on-the-fly syntax checking extension
;; - tldr - practical help pages for CLI tools
                                        ; - devdocs - searchable documentation from devdoc
;; - treesit-auto - syntax highlighting using tree-sitter
;;; Code:


(defgroup neatmacs-development nil
  "Neatmacs Development Configuration"
  :group 'neatmacs
  :prefix 'neatmacs-development-
  )

(defun neatmacs-development--message (msg)
  "Messages the passed argument MSG with a prefix of [Neat-Development]."
  (message "[NEAT] (Development) %s" msg))

(defvar neatmacs-development-settings '((show-trailing-whitespace . t))
  "Settings to apply in `neatmacs-development-apply-settings`.")

(defun neatmacs-development-apply-settings ()
  "Apply development settings."
  (dolist (setting neatmacs-development-settings)
    (let ((var (car setting))
          (val (cdr setting)))
      ;; (neatmacs-development--message (format "Setting %s to %s" var val))
      (set var val))))

(add-hook 'prog-mode-hook #'neatmacs-development-apply-settings)


;;================= Super completion at point function ================

(defcustom neatmacs-development-super-capfs '(tempel-complete
                                              eglot-completion-at-point
                                              cape-file)
  "Completion at point functions to use in `neatmacs-development-super-capf-mode`."

  :type '(list)
  :group 'neatmacs-development)

(define-minor-mode neatmacs-development-super-capf-mode
  "Toggle super-capf mode."
  :init-value t
  :global nil
  :group 'neatmacs-development

  (when (derived-mode-p 'prog-mode)

    (defalias 'neatmacs-development-capf
      (apply #'cape-super-capf neatmacs-development-super-capfs))

    (if neatmacs-development-super-capf-mode
        (setq-local completion-at-point-functions
                    (cons
                     #'neatmacs-development-capf
                     completion-at-point-functions))
      (setq-local completion-at-point-functions
                  (remove  #'neatmacs-development-capf
                           completion-at-point-functions)))))


;;========================== LANGUAGE SERVER ==========================

(use-package eglot
  :straight (:type built-in)
  :hook (prog-mode . eglot-ensure)
  :custom
  ;; (eglot-events-buffer-size 0)
  ;; (read-process-output-max (* 1024 1024))
  ;; (eglot-ignored-server-capabilities '(:hoverProvider
  ;;                                      :documentHighlightProvider))
  (eglot-extend-to-xref t)
  (eglot-autoshutdown t)
  :init
  ;; Only useful in some cases
  ;; (with-eval-after-load 'cape
  ;;   (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))
  :config
  ;; Use orderless for eglot completion
  (add-to-list 'completion-category-overrides '(eglot (styles orderless partial-completion basic))))


;;================================ GIT ================================

(use-package magit
  :bind ("C-x g" . magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Display keyword entries from source code comments and Org files in the Magit status buffer.
(use-package magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode))

;; Repository statistics
(use-package magit-stats
  :after magit)

;; Git gists inside Emacs
(use-package igist
  :straight (igist :repo "KarimAziev/igist" :type git :host github))

;; Highlight diffs in the fringe
(use-package diff-hl
  :hook (prog-mode . diff-hl-mode)
  :config
  (diff-hl-flydiff-mode)
  (global-diff-hl-mode))


;;=========================== DOCUMENTATION ===========================

(use-package eldoc
  :custom
  ;; (eldoc-echo-area-use-multiline-p nil)   ; Don't span multiple lines
  (eldoc-echo-area-use-multiline-p t)   ; Don't span multiple lines
  (eldoc-idle-delay 0.75)               ; Increase the delay before eldoc is displayed
  :hook prog-mode)

;;; Provides practical help pages for CLI tools
(use-package tldr)

;;; Provides searchable documentation from devdoc
(use-package devdocs)


;;============================ TREE-SITTER ============================

(use-package combobulate)

;; (use-package treesit-auto
;;   :if (version<= "29" emacs-version)
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (global-treesit-auto-mode))


;; (when (version< emacs-version "29")
;;   (message "Installing development packages built-in in later versions of Emacs < 29.")

;;   (use-package tree-sitter)
;;   (use-package tree-sitter-indent)
;;   (use-package tree-sitter-ispell)
;;   (use-package tree-sitter-langs)
;;   )

;; (use-package treesit
;;   :when (version<= "29" emacs-version)
;;   :straight (treesit :type built-in)
;;   :custom (treesit-language-source-alist
;;            '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;              (cmake "https://github.com/uyha/tree-sitter-cmake")
;;              (css "https://github.com/tree-sitter/tree-sitter-css")
;;              (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;              (go "https://github.com/tree-sitter/tree-sitter-go")
;;              (html "https://github.com/tree-sitter/tree-sitter-html")
;;              (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;              (json "https://github.com/tree-sitter/tree-sitter-json")
;;              (make "https://github.com/alemuller/tree-sitter-make")
;;              (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;              (python "https://github.com/tree-sitter/tree-sitter-python")
;;              (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;              (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
;;              (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;              (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))


(use-package markdown-mode)

;;============================= TEMPLATES =============================

(use-package tempel
  :after eglot)

(use-package tempel-collection
  :after tempel)

(use-package license-templates)
(use-package gitignore-templates)


;;============================= PRETTINESS ============================

(use-package pretty-mode
  :hook prog-mode
  :config
  (pretty-deactivate-groups
   '(:equality :ordering :ordering-double :ordering-triple :arrows :arrows-twoheaded :punctuation :logic :sets))

  (pretty-activate-groups
   '(:sub-and-superscripts :greek :arithmetic-nary)))

;; Highlight TODOs and other keywords in comments and strings
;; TODO does not highlight the the last three. Seems like modus themes overrides the faces.
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :custom
  (hl-todo-keyword-faces '(("TODO" . "#FF0000")
                           ("FIXME" . "#FF0000")
                           ("DEBUG" . "#A020F0")
                           ("GOTCHA" . "#FF4500")
                           ("STUB" . "#1E90FF"))))


;;=========================== UNCATEGORIZED ===========================

;; TODO: Remove. Overall prefer show-paren-mode
;; (use-package rainbow-delimiters
;;   :hook (prog-mode . rainbow-delimiters-mode))

;; TODO: Look into
;; (use-package editorconfig
;;   :hook (prog-mode . editorconfig-mode))

;; TODO: Look into
;; (use-package ibuffer-project
;;   :after ibuffer project)

;; TODO: Look into
;; Frontend for Debug Adapter Protocol
;; (use-package realgud)

;; TODO: Remove. Use banner-comment instead
;; (use-package line-comment-banner)
(use-package banner-comment)

(use-package flycheck
  :straight (:type git :host github :repo "flycheck/flycheck")
  :hook (prog-mode . flycheck-mode))

;; TODO: Requires rg
(use-package deadgrep)

;; Intelligently cleans up whitespaces on save
(use-package whitespace-cleanup-mode
  :hook (prog-mode . whitespace-cleanup-mode))

(use-package ediff
  :straight (:type built-in)
  :config
  (defvar neatmacs-development--ediff-last-windows nil
    "Last ediff window configuration.")

  (defun neatmacs-development--store-pre-ediff-winconfig ()
    "Store the window configuration before ediff."
    (setq neatmacs-development--ediff-last-windows (current-window-configuration)))

  (defun neatmacs-development--restore-pre-ediff-winconfig ()
    "Restore the window configuration after ediff."
    (set-window-configuration neatmacs-development--ediff-last-windows))

  ;; Restore window configuration after quitting ediff
  (add-hook 'ediff-before-setup-hook #'neatmacs-development--store-pre-ediff-winconfig)
  (add-hook 'ediff-quit-hook #'neatmacs-development--restore-pre-ediff-winconfig))



(provide 'neat-development)
;;; neat-development.el ends here
