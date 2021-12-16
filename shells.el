(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "bash")
  (setq explicit-zsh-args '()))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))


(defun linas/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size 2000           ;;Default 10000
        eshell-buffer-maximum-lines 2000           ;;Default 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

;; Powerline makes it look cooler
(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . linas/configure-eshell)
  :config
    (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim" "nvim" "nano")))
  (eshell-git-prompt-use-theme 'powerline))

;; Not using anymore

;; (use-package vterm
;;   :commands vterm
;;   :config
;;   (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
;;   (setq vterm-shell "zsh")
;;   (setq vterm-max-scrollback 3000)) ;; Default 10000, set to 3000 in case of lag

(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))

(add-hook 'shell-mode-hook
          (lambda ()
            ;; Disable font-locking in this buffer to improve performance
            (font-lock-mode -1)
            ;; Prevent font-locking from being re-enabled in this buffer
            (make-local-variable 'font-lock-function)
            (setq font-lock-function (lambda (_) nil))
            (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

(use-package shell-pop
  ;; :bind ("C-c s" . shell-pop)
  :custom
  (shell-pop-default-directory "~/")
  (shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (shell-pop-term-shell "zsh")
  (shell-pop-universal-key "C-c s")
  (shell-pop-window-size 25)
  (shell-pop-full-span t)
  (shell-pop-window-positioning "bottom")
  (shepp-pop-autocd-to-working-dir t)
  (shell-pop-restore-window-configuration t)
  (shell-pop-cleanup-buffer-at-process-exit t))
