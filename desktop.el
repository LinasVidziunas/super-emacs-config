(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package desktop-environment
  :after exwm
  :config

  ;; Rebinding dekstop-environment-lock-screen form s-l to s-L for integration with windowmove keybinds
  (define-key desktop-environment-mode-map (kbd "s-l") nil)
  (define-key desktop-environment-mode-map (kbd "s-L") #'desktop-environment-lock-screen)

  (desktop-environment-mode)
  :custom
  ;; Brightness increments and decrements
  (desktop-environment-brightness-small-increment "1%+")
  (desktop-environment-brightness-small-decrement "1%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-")

  ;; Volume increments and decrements
  (desktop-environment-volume-small-increment "1%+")
  (desktop-environment-volume-small-decrement "1%-")
  (desktop-environment-volume-normal-increment "5%+")
  (desktop-environment-volume-normal-decrement "5%-"))

(defvar linas/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun linas/kill-panel ()
  (interactive)
  (when linas/polybar-process
    (ignore-errors
      (kill-process linas/polybar-process))))

(defun linas/start-panel ()
  (interactive)
  (linas/kill-panel)
  (setq linas/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

(defun linas/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun linas/send-polybar-exwm-workspace ()
  (linas/send-polybar-hook "exwm-workspace" 1))

;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'linas/send-polybar-exwm-workspace)

(defun linas/dunstctl (command)
  (start-process-shell-command "dunstctl" nil (concat "dunstctl " command)))

(defun linas/desktop-notifications-enable ()
  (interactive)
  (linas/dunstctl "set-paused false"))

(defun linas/desktop-notifications-disable ()
  (interactive)
  (linas/dunstctl "set-paused true"))

(defun linas/desktop-notifications-toggle ()
  (interactive)
  (linas/dunstctl "set-paused toggle"))

(defun linas/switch-keyboard-layout (to_layout)
  "Function to switch between keyboard layouts"
  (interactive
   (list (completing-read "MSwitch keybaord layout to: "
                          '(("us") ("no") ("lt")) nil t)))
  (start-process-shell-command "setxkbmap" nil (format "setxkbmap %s" to_layout)))

;; later use for polybar to display keyboard layout
(defun linas/get-keyboard-layout ()
  (interactive))

(use-package ncla
  :straight '(NCLA :host github :repo "LinasVidziunas/NCLA"))

(defun linas/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun linas/exwm-init-hook ()
  ;;   Make workspace 1 to be the one where we land at startup
  ;;   (exwm-workspace-switch-create 1)

  ;;   Open eshell by defaul
  ;;   (eshell)

  ;;Launch apps that will run in the background
  (linas/run-in-background "nm-applet")
  (linas/run-in-background "pasystray")
  (linas/run-in-background "blueman-applet")
  (linas/run-in-background "dunst"))

;; Start the Polybar panel
(linas/start-panel)

(defun linas/exwm-update-class ()
   (exwm-workspace-rename-buffer exwm-class-name))

(defun linas/exwm-update-title-firefox-remove-double-name ()
  (if (string-match-p " — Mozilla Firefox\\'" exwm-title)
      (substring exwm-title 0 (string-match-p " — Mozilla Firefox\\'" exwm-title))
    exwm-title))

;; Not finished, maninly cuz im retarded
;; (defun linas/exwm-update-title-firefox ()
;;   (interactive)
;; (message (length (linas/exwm-update-title-firefox-remove-double-name)))
;;   (if (> (length linas/exwm-update-title-firefox-remove-double-name) 60)
;;       (concat (substring exwm-title 0 60) "...")
;;     (linas/exwm-update-title-firefox-remove-double-name)))

(defun linas/exwm-update-title ()
  (pcase exwm-class-name
    ("Firefox"
     (exwm-workspace-rename-buffer
      (format "Firefox: %s" (linas/exwm-update-title-firefox-remove-double-name))))))

(defun linas/configure-window-by-class ()
  (interactive)
  ;; (message "Window '%s' appeared!" exwm-class-name)
  (pcase exwm-class-name
    ("mpv" (exwm-floating-toggle-floating)
     (exwm-layout-toggle-mode-line))
    ("Firefox" (exwm-workspace-move-window 0))))

(defun linas/position-window ()
  (let* ((pos (frame-position)
           (pos-x (car pos))
           (pos-y (cdr pos)))

        (exwm-floating-move (- pos-x) (- pos-y)))))

(defun linas/set-wallpaper ()
  (interactive)
  (start-process-shell-command
   "nitrogen" nil "nitrogen --restore"))

(use-package exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 10)

  ;; Automatically move EXWM buffer to current workspace when slected
  ;; exwm-switch-to-buffer 
  ;; (setq exwm-layout-show-all-buffers t)

  ;; Display all EXWM buffers in every workspace buffer list
  ;; (setq exwm-workspace-show-all-buffers t) 

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'linas/exwm-update-class)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'linas/exwm-update-title)

  ;; When EXWM starts up, do some extra configuration
  (add-hook 'exwm-init-hook #'linas/exwm-init-hook)

  ;; Set the screen resolution
  (require 'exwm-randr)
  (exwm-randr-enable)
  (start-process-shell-command "xrandr" nil "xrandr --output eDP1 --primary --mode 1920x1080 --pos 0x0 --rotate normal")

  (linas/set-wallpaper)

  ;; Load the system tray before exwm-init
  ;; Commented out because using polybar
  ;; (require 'exwm-systemtray)
  ;; (setq exwm-systemtray-height 20)
  ;; (exwm-systemtray-enable)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j  ;; Buffer list
          ?\C-\ ))  ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([?\s-h] . windmove-left)
          ([?\s-l] . windmove-right)
          ([?\s-k] . windmove-up)
          ([?\s-j] . windmove-down)

          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ;;([?\s-w] . exwm-workspace-switch)

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-input-set-key (kbd "s-SPC") 'ncla)
  (exwm-input-set-key (kbd "s-f") 'exwm-layout-toggle-fullscreen)

  (exwm-input-set-key (kbd "s-n") (lambda () (interactive) (linas/dunstctl "history-pop")))
  (exwm-input-set-key (kbd "s-N") (lambda () (interactive) (linas/dunstctl "close-all")))

  (exwm-enable))

(defun linas/lookup-password (&rest keys)
  (interactive)
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

(setq epg-pinentry-mode 'loopback)

(use-package mu4e
  :straight nil
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :defer 10 ; Wait until 10 seconds after startup
  :config

  ;; Pull in org helpers
  (require 'mu4e-org)

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Refresh mail using isync every 2 minutes
  (setq mu4e-update-interval (* 2 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/Mail")

  (setq mu4e-contexts
        (list
         ;; Main personal account
         (make-mu4e-context
          :name "linasvidz@gmail.com"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/linasvidz@gmail.com" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "linasvidz@gmail.com")
                  (user-full-name    . "Linas Vidziunas")
                  (mu4e-compose-signature  . "Linas Vidziunas")
                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-service  . 465)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder  . "/linasvidz@gmail.com/[Gmail]/Drafts")
                  (mu4e-sent-folder  . "/linasvidz@gmail.com/[Gmail]/Sent Mail")
                  (mu4e-refile-folder  . "/linasvidz@gmail.com/[Gmail]/All Mail")
                  (mu4e-trash-folder  . "/linasvidz@gmail.com/[Gmail]/Trash")))

         ;; School account
         (make-mu4e-context
          :name "school"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/school" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "254664@uis.no")
                  (user-full-name    . "Linas Vidziunas")
                  (mu4e-compose-signature  . "Linas Vidzinas (254664)")
                  (smtpmail-smtp-server  . "smtp.office365.com")
                  (smtpmail-smtp-service  . 587)
                  (smtpmail-stream-type  . starttls)
                  (mu4e-drafts-folder  . "/school/Drafts")
                  (mu4e-sent-folder  . "/school/Sent Mail")
                  (mu4e-refile-folder  . "/school/All Mail")
                  (mu4e-trash-folder  . "/school/Trash")))))

  ;; Email Capture Templates
  (setq org-capture-templates
        `(("m" "Email Workflow")
          ("mf" "Follow Up" entry (file+olp "~/org/Mail.org" "Follow Up")
           "* TODO Follow up with %:fromname on %a\n    SCHEDULED:%t\n    DEADLINE:%(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n    <%:fromaddress> on %:date-timestamp\n\n\t%a\n\n\t%i"
           :immediate-finish t)
          ("mr" "Read Later" entry (file+olp "~/org/Mail.org" "Read Later")
           "* TODO Read %a\n    SCHEDULED:%t\n    DEADLINE:%(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n    <%:fromaddress> on %:date-timestamp\n\n\t%a\n\n\t%i"
           :immediate-finish t)))

  (defun linas/capture-mail-follow-up (msg)
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "mf"))

  (defun linas/capture-mail-read-later (msg)
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "mf"))

  (defun linas/store-link-to-mu4e-query ()
    (interactive)
    (let ((mu4e-org-link-query-in-headers-mode t))
      (call-interactively 'org-store-link)))

  ;; Add custom actions for our capture templates
  (add-to-list 'mu4e-headers-actions
               '("follow up" . linas/capture-mail-follow-up) t)
  (add-to-list 'mu4e-view-actions
               '("follow up" . linas/capture-mail-follow-up) t)
  (add-to-list 'mu4e-headers-actions
               '("read later" . linas/capture-mail-read-later) t)
  (add-to-list 'mu4e-view-actions
               '("read later" . linas/capture-mail-read-later) t)

  (setq mu4e-maildir-shortcuts
        '((:maildir "/linasvidz@gmail.com/Inbox"    :key ?i)
          (:maildir "/linasvidz@gmail.com/[Gmail]/Sent Mail" :key ?s)
          (:maildir "/linasvidz@gmail.com/[Gmail]/Trash"     :key ?t)
          (:maildir "/linasvidz@gmail.com/[Gmail]/Drafts"    :key ?d)
          (:maildir "/linasvidz@gmail.com/[Gmail]/All Mail"  :key ?a)))

  ;; Annoyingly the first mail has to be sent synchrounously
  (setq send-mail-function 'smtpmail-send-it)
  (setq message-send-mail-function 'smtpmail-send-it)

  ;; My attemt at async mail
  ;; (setq linas/smtp-to-async-smtp-mails-sent 0)
  ;; (defun linas/smtp-to-async-smtp ()
  ;;   (message "Message sent synchronously")
  ;;   ;; Why 2? how the fuck im I supposed to know
  ;;   (when (> linas/smtp-to-async-smtp-mails-sent 2)
  ;;     (require 'smtpmail-async)
  ;;     (message "Message sent asynchronously")
  ;;     (setq send-mail-function 'async-smtpmail-send-it)
  ;;     (setq message-send-mail-function 'async-smtpmail-send-it))
  ;;   (setq linas/smtp-to-async-smtp-mails-sent (+ linas/smtp-to-async-smtp-mails-sent 1)))

  ;; (add-hook 'message-sent-hook #'linas/smtp-to-async-smtp)

  ;; When enters main view picks the first context (first email)
  (setq mu4e-context-policy 'pick-first)

  ;; Make sure plain text mails flow correctly for recipients
  (setq mu4e-compose-format-flowed t)

  ;; Defaults
  ;; (setq mu4e-date-format-long "%c")
  ;; (setq mu4e-view-date-format "%c")
  ;; (setq mu4e-headers-long-date-format "%c")
  ;; (setq mu4e-headers-date-format "%x")
  (setq mu4e-headers-date-format "%d/%m/%y")

  (mu4e t))

(use-package mu4e-column-faces
  :after mu4e
  :config (mu4e-column-faces-mode))

(use-package mu4e-alert
  :after mu4e
  :custom
  ;; Disable double notifications per email. annoying
  (mu4e-alert-email-notification-types '(subjects))
  :config
  (mu4e-alert-set-default-style 'libnotify)
  (mu4e-alert-enable-notifications))

;; mu4e-alert fix
;; Annoying but looks like dev is dead
(defun mu4e-alert--get-mu4e-frame ()
  "Try getting a frame containing a mu4e buffer."
  (car (delq nil (mapcar (lambda (buffer)
                           (when (and buffer
                                      (get-buffer-window buffer t))
                             (window-frame (get-buffer-window buffer t))))
                         (list mu4e-main-buffer-name)))))

(defun mu4e-alert-filter-repeated-mails (mails)
  "Filters the MAILS that have been seen already."
  (cl-remove-if (lambda (mail)
                  (prog1 (and (not mu4e-alert-notify-repeated-mails)
                              (ht-get mu4e-alert-repeated-mails
                                      (plist-get mail :message-id)))
                    (ht-set! mu4e-alert-repeated-mails
                             (plist-get mail :message-id)
                             t)
                    ))
                mails))

(use-package org-mime
  :after mu4e
  :custom
  (org-mime-export-options '(:section-numbers nil
                                              :with-author nil
                                              :with-toc nil))
  :config
  (add-hook 'message-send-hook 'org-mime-confirm-when-no-multipart))

(defun linas/proced-settings ()
  (setq proced-format (list 'pid 'tree 'pcpu 'pmem (list 'args 'comm)))
  (proced-toggle-tree t)
  (proced-toggle-auto-update t))

(use-package proced
  :straight nil
  :config
  (setq proced-auto-update-interval 1)
  (add-hook 'proced-mode-hook 'linas/proced-settings))
