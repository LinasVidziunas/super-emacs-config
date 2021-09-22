(server-start)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(setq package-enable-at-startup nil)

(defvar linas/default-font-size 120)
(defvar linas/default-variable-font-size 120)
(defvar linas/frame-transparency '(90 . 90))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)		; Disable visable scrollbar
(tool-bar-mode -1)		; Disable the toolbar
(tooltip-mode -1)		; Disable tooltips
(set-fringe-mode 10)		; Give some breathing room

(menu-bar-mode -1)		; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(set-frame-parameter (selected-frame) 'alpha linas/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,linas/frame-transparency))

;; Enabling line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable the line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                vterm-mode-hook
                mu4e-main-mode-hook
                mu4e-headers-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

(defun linas/set-font-faces ()
  ;; Font
  ;;(set-face-attribute 'default nil :font "Jetbrains Mono" :height default-font-size)
  (set-face-attribute 'default nil :font "Fira Code Retina" :height linas/default-font-size)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height linas/default-font-size)
  ;;(set-face-attribute 'fixed-pitch nil :font "Jetbrains Mono" :height default-font-size)
  :;(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height default-font-size)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height linas/default-variable-font-size :weight 'regular))

;; Not working for some reason
;; (if (deamonp)
;;     (add-hook 'after-make-frame-functions
;;               (lambda (frame)
;;                 (setq doom-modeline-icon t)
;;                 (with-selected-frame frame
;;                   (linas/set-font-faces))))
;;   (linas/set-font-faces))

(linas/set-font-faces)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :after evil
  :config
  (general-create-definer linas/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (linas/leader-keys
   "t" '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme")

   "o" '(:ignore t :which-key "open")
   "op" '(treemacs :which-key "Treemacs")

   ;; Open shell
   "os" '(:ignore t :which-key "shells")
   "ose" '(eshell :which-key "Eshell")

   "x" '(:ignore t :which-key "exwm")
   "xh" '(:ignore t :which-key "horizontal size")
   "xhk" '((lambda () (interactive) (exwm-layout-enlarge-window-horizontally 100)) :which-key "enlarge +100")
   "xhj" '((lambda () (interactive) (exwm-layout-shrink-window-horizontally 100)) :which-key "shrink +100")
   ))

;; Evil mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :custom
  (evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

(setq-default bidi-paragraph-direction 'left-to-right)

(if (version<= "27.1" emacs-version)
    (setq bidi-inhibit-bpa t))

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;;(setq make-backup-files nil)

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/"
                                                   user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves"
                                                                user-emacs-directory) t)))

;; (setq create-lockfiles nil)

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;No use currently
;(use-package command-log-mode)

(use-package doom-themes
  :init (load-theme 'doom-gruvbox t)) ;; Doom themes

(use-package all-the-icons
  :init
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install fonts t))
  :custom
  (all-the-icons-scale-factor 1))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package which-key
  :defer t
  :init (which-key-mode)
  :diminish which-key-mode

  ;; Setting to 300ms to hopefully decrease CPU usage
  :config
  (setq which-key-idle-delay 300))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-M-j" . counsel-switch-buffer)
         ("C-s-j" . counsel-switch-buffer)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-pretty)
  :config
  (counsel-mode 1))

(use-package ivy
  :after counsel
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1))

;; Might not need, just looking for icons tbh
(use-package all-the-icons-ivy
  :after ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package ivy-prescient
  :after counsel ivy
  :custom
  ;; If below set to nil, then ivy-prescient.el does not apply prescient.el filtering to Ivy, but will still sort.
  (ivy-prescient-enable-filtering t)

  ;; Ivy prescient changes how the results are highlighted. To emulate old highlighting you can set this to true.
  (ivy-prescient-retain-classic-highlighting t)

  (prescient-filter-method '(literal regexp anchored))
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package company-prescient
  :after company
  :custom
  ;; Do not sort after length of the candidate
  (company-prescient-sort-length-enable nil)
  :config
  (company-prescient-mode 1))

(use-package helpful
  :after counsel
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-decrease "out")
  ("k" text-scale-increase "in")
  ("l" nil "finished" :exit t))

(linas/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package pretty-mode
  :hook (prog-mode . pretty-mode) ;; For all programming modes
  :config
    (pretty-deactivate-groups
      '(:equality :ordering :ordering-double :ordering-triple
        :arrows :arrows-twoheaded :punctuation
        :logic :sets))

    (pretty-activate-groups
      '(:sub-and-superscripts :greek :arithmetic-nary)))

;; (use-package shackle
;;   :custom
;;   (shackle-rules
;;    '((help-mode :noselect t)
;;      (helpful-mode :noselect t)
;;      (magit-mode :noselect t)
;;      ))
;;   (shackle-default-rule '(:same t))
;;   :config
;;   (shackle-mode 1))

(use-package undo-fu)

(use-package undo-fu-session
  :custom
  (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config
  (global-undo-fu-session-mode))

(use-package async
  :custom
  ;; Compile all packages asynchronously
  (async-bytecomp-allowed-packages 'all)
  :config
  ;; This will allow you to run asynchronously the dired commands for copying, renaming and symlinking. If you are a helm user, this will allow you to copy, rename etc... asynchronously from helm. Note that with helm you can disable this by running the copy, rename etc... commands with a prefix argument.
  (dired-async-mode 1)
  ;; Compile packages asynchronously
  (async-bytecomp-package-mode 1))

(defun linas/org-mode-setup ()
    (org-indent-mode)
    (variable-pitch-mode 1)
    (visual-line-mode 1)
    (auto-fill-mode 0)) ; might need to remove the last

(defun linas/org-font-setup ()
    ;; Replace list hyphen with dot
    (font-lock-add-keywords 'org-mode
                            '(("^ *\\([-]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

    ;; Set faces for heading levels
    (dolist (face '((org-level-1 . 1.2)
                    (org-level-2 . 1.1)
                    (org-level-3 . 1.05)
                    (org-level-4 . 1.0)
                    (org-level-5 . 1.1)
                    (org-level-6 . 1.1)
                    (org-level-7 . 1.1)
                    (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
    (setq evil-auto-indent nil)) ; might need to take out this later

(use-package org
  :commands (org-capture org-agenda)
  :hook (org-mode . linas/org-mode-setup)
  :straight (:type built-in)
  :config
  (setq org-ellipsis " ▾") ; ... to the triangle thingy

  (setq org-pretty-entities t)
  (setq org-hide-emphasis-markers t)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files '("~/org/Tasks.org"
                           "~/org/Mail.org"))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
        '(("Archive.org" :maxlevel . 1)
          ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
        '((:startgroup)
                                        ; Put mutually exclusive tags here
          (:endgroup)
          ("@errand" . ?E)
          ("@home" . ?H)
          ("@work" . ?W)
          ("agenda" . ?a)
          ("planning" . ?p)
          ("publish" . ?P)
          ("batch" . ?b)
          ("note" . ?n)
          ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ("W" "Work Tasks" tags-todo "+work")

          ;; Low-effort next actions
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))

          ("w" "Workflow Status"
           ((todo "WAIT"
                  ((org-agenda-overriding-header "Waiting on External")
                   (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "In Review")
                   (org-agenda-files org-agenda-files)))
            (todo "PLAN"
                  ((org-agenda-overriding-header "In Planning")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "BACKLOG"
                  ((org-agenda-overriding-header "Project Backlog")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "READY"
                  ((org-agenda-overriding-header "Ready for Work")
                   (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Active Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
                  ((org-agenda-overriding-header "Completed Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "CANC"
                  ((org-agenda-overriding-header "Cancelled Projects")
                   (org-agenda-files org-agenda-files)))))))

  (linas/org-font-setup))

;; Automatically tangle our Emacs.org config file when we save it
(defun linas/org-babel-tangle-config ()

 (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name "~/Projects/super-emacs-config/"))

    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'linas/org-babel-tangle-config)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun linas/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . linas/org-mode-visual-fill))

;; This is needed as of Org 9.2
(with-eval-after-load 'org
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/RoamNotes")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("c" "chemistry" plain (file "~/RoamNotes/Templates/ChemestryStudyNotes.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
   :bind (("C-c n l" . org-roam-buffer-toggle)
          ("C-c n f" . org-roam-node-find)
          ("C-c n i" . org-roam-node-insert)
          :map org-mode-map
          ("C-M-i"    . completion-at-point))
   :config
   (org-roam-setup))

(use-package dired
  :straight (:type built-in)
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
    (evil-collection-define-key 'normal 'dired-mode-map
      "h" 'dired-up-directory
      "l" 'dired-find-file))

(use-package dired-single
  :after dired)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :commands (dired dired-jump)
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package treemacs
  :commands treemacs
  :custom
  (treemacs-width 30))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :after dired
  :config
  (treemacs-icons-dired-mode)
  :custom
  (treemacs--icon-size 1))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package auth-source
  :defer t
  :custom 
  (auth-source-pass-filename "~/.password-store/"))

(use-package pass)

;; Make gc pauses faster by decresing the threshold
(setq gc-cons-threshold (* 2 1000 1000))

(if (file-exists-p "~/.emacs.d/development.el")
    (load-file "~/.emacs.d/development.el")
  (message "Development packages not loading. development.el file not found in the emacs directory"))

(if (file-exists-p "~/.emacs.d/shells.el")
    (load-file "~/.emacs.d/shells.el")
  (message "Shells packages not loading. shells.el file not found in the emacs directory"))
