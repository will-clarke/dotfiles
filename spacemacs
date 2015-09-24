;; -*- mode: emacs-lisp -*-
(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   dotspacemacs-configuration-layers
   '(
     (ruby :variables
           ruby-version-manager 'rbenv
           ruby-enable-ruby-on-rails-support t
           enh-ruby-add-encoding-comment-on-save nil)
     html
     auto-completion
     better-defaults
     emacs-lisp
     (git :variables
          git-magit-status-fullscreen t
          git-enable-github-support t
          git-gutter-use-fringe t)
     markdown
     osx
     org
     perspectives
     (shell :variables
            shell-default-term-shell "/bin/bash"
            shell-default-shell 'eshell
            shell-default-position 'bottom
            shell-default-height 50)
     syntax-checking
     version-control
     dash
     eyebrowse
     games
     github
     emoji
     gtags
     xkcd
     scala
     emacs-lisp
     rust
     (python :variables
             python-enable-yapf-format-on-save t)

     mu4e

     )
   dotspacemacs-additional-packages '(soft-charcoal-theme
                                      color-theme-sanityinc-tomorrow
                                      twittering-mode
                                      sx
                                      gnugo
                                      w3m
                                      csv-mode
                                      ;; evil-rails
                                      ;; racer
                                      ;; exec-path-from-shell
                                      )

   dotspacemacs-excluded-packages '()
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."

  (setq-default
   ruby-version-manager 'rbenv
   ruby-enable-ruby-on-rails-support t
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'random ;;'official
   dotspacemacs-startup-lists '(recents projects bookmarks)
   dotspacemacs-themes '(solarized-light
                         soft-charcoal
                         sanityinc-tomorrow-bright

                         )
   ;; solarized-dark
   ;; spacemacs-light
   ;; spacemacs-dark
   ;; leuven
   ;; monokai
   ;; zenburn)

   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 23
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-command-key ":"
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-use-ido nil
   dotspacemacs-enable-paste-micro-state t
   dotspacemacs-guide-key-delay 0.4
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup t
   dotspacemacs-fullscreen-use-non-native t
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible value is `all',
   ;; `current' or `nil'. Default is `all'
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("ag" "ack" "grep")
   dotspacemacs-default-package-repository nil
   )
  ;; User initialization goes here
  )

(defun dotspacemacs/config ()

  ;; Startup
  ;; (mu4e)
  ;; (switch-to-buffer "*spacemacs*")

  (defun source (filename)
    "Update environment variables from a shell source file."
    (interactive "fSource file: ")

    (message "Sourcing environment from `%s'..." filename)
    (with-temp-buffer

      (shell-command (format "diff -u <(true; export) <(source %s; export)" filename) '(4))

      (let ((envvar-re "declare -x \\([^=]+\\)=\\(.*\\)$"))
        ;; Remove environment variables
        (while (search-forward-regexp (concat "^-" envvar-re) nil t)
          (let ((var (match-string 1)))
            (message "%s" (prin1-to-string `(setenv ,var nil)))
            (setenv var nil)))

        ;; Update environment variables
        (goto-char (point-min))
        (while (search-forward-regexp (concat "^+" envvar-re) nil t)
          (let ((var (match-string 1))
                (value (read (match-string 2))))
            (message "%s" (prin1-to-string `(setenv ,var ,value)))
            (setenv var value)))))
    (message "Sourcing environment from `%s'... done." filename))

  (source "~/.secrets")

  ;; Mac
  (setq vc-follow-symlinks t)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)

  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

  ;; Yasnippets
  (setq yas-snippet-dirs '("/Users/wmmc/.emacs.d/snippets/" "~/.emacs.d/private/snippets" yas-installed-snippets-dir ))

  ;; gk and gj are sensible
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  ;; dired has a groovy way of going up directories
  (evil-declare-key 'normal dired-mode-map (kbd ";") 'dired-up-directory)

  ;; Twitter
  (setq twittering-use-master-password t)

  ;; Go
  (evil-declare-key 'motion gnugo-board-mode-map (kbd "<return>") 'gnugo-move)
  (evil-declare-key 'motion gnugo-board-mode-map (kbd "RET") 'gnugo-move)
  (evil-declare-key 'motion gnugo-board-mode-map (kbd "q") 'gnugo-quit)
  ;; backspace
  (global-set-key (kbd "C-?") 'help-command)
  (global-set-key (kbd "M-?") 'mark-paragraph)
  (global-set-key (kbd "C-h") 'delete-backward-char)
  (global-set-key (kbd "M-h") 'backward-kill-word)

  ;; Disable smartparens for most pairs
  ;; (eval-after-load 'smartparens
  ;;   '(progn
  ;;      (sp-pair "(" nil :actions :rem)
  ;;      (sp-pair "[" nil :actions :rem)
  ;;      (sp-pair "'" nil :actions :rem)
  ;;      (sp-pair "\"" nil :actions :rem)))

  ;; OSX commands
  (global-set-key (kbd "M-v") 'yank)
  (global-set-key (kbd "M-c") 'evil-yank)
  (global-set-key (kbd "M-a") 'mark-whole-buffer)
  (global-set-key (kbd "M-w") 'delete-window)
  (global-set-key (kbd "M-n") 'split-window-right)
  (global-set-key (kbd "M-s")
                  (lambda ()
                    (interactive)
                    (call-interactively (key-binding "\C-x\C-s"))))

  ;; rust racer
  (setq rust-enable-racer t)
  (setenv "RUST_SRC_PATH" "/Users/wmmc/.rust/src/")
  (setq racer-rust-src-path "/Users/wmmc/.rust/src/")

  ;; create loads of shells
  (defun make-shell (name)
    "Create a shell buffer named NAME."
    (interactive "sName: ")
    (setq name (concat "$" name))
    (eshell)
    (rename-buffer name))

  ;; Eshell aliases
  (require 'em-alias)
  (setq eshell-aliases-file "~/.emacs.d/aliases")

  ;; Email
  (setq mu4e-account-alist
        '(("gmail"
           (mu4e-sent-messages-behavior delete)
           (mu4e-sent-folder "/gmail/Sent")
           (mu4e-refile-folder  "/gmail/Archive")
           (mu4e-trash-folder  "/gmail/Trash")
           (mu4e-follow-up-folder  "/gmail/Later")
           (mu4e-drafts-folder "/gmail/Drafts")
           (user-mail-address "wmmclarke@gmail.com")
           (user-full-name "William"))
          ("snaptrip"
           (mu4e-sent-messages-behavior delete)
           (mu4e-sent-folder "/snaptrip/Sent")
           (mu4e-refile-folder  "/snaptrip/Archive")
           (mu4e-trash-folder  "/snaptrip/Trash")
           (mu4e-follow-up-folder  "/snaptrip/Later")
           (mu4e-drafts-folder "/snaptrip/Drafts")
           (user-mail-address "will.clarke@snaptrip.com")
           (user-full-name "Will Clarke"))))
  (mu4e/mail-account-reset)

  ;; (require 'starttls)
  (setq
   mu4e-maildir "~/.mail"
   mu4e-trash-folder "/trash"
   mu4e-refile-folder "/archive"

   mu4e-get-mail-command "mbsync Inboxes; mbsync -a"
   ;; 900 second = 15 minutes
   mu4e-update-interval 900
   mu4e-compose-signature-auto-include nil
   mu4e-view-show-images t
   mu4e-view-show-addresses t

   mu4e-html2text-command "w3m -dump -T text/html"
   mu4e-view-prefer-html t
   mu4e-use-fancy-chars t

   ;; use msmtp
   message-send-mail-function 'message-send-mail-with-sendmail
   sendmail-program "/usr/local/bin/msmtp"
   ;; tell msmtp to choose the SMTP server according to the from field in the outgoing email
   message-sendmail-extra-arguments '("--read-envelope-from")
   message-sendmail-f-is-evil 't
   )

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (setq mu4e-maildir-shortcuts
        '(("/gmail/Inbox" . ?g)
          ("/snaptrip/Inbox" . ?s)
          ("/snaptrip/Later" . ?w)
          ("/gmail/Later" . ?l)))

  (setq mu4e-bookmarks
        `(
          ("maildir:/gmail/Inbox OR maildir:/snaptrip/Inbox" "All Inboxes" ?i)
          ("maildir:/gmail/Later OR maildir:/snaptrip/Later" "All Later" ?l)
          ("date:today..now" "Today's messages" ?t)
          ("date:today..now AND maildir:/snaptrip/Archive" "Snaptrip Today" ?s)
          ("date:today..now AND maildir:/gmail/Archive" "Gmail Today" ?g)
          ("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
          ("date:7d..now" "Last 7 days" ?w)
          ("mime:image/*" "Messages with images" ?p)
          (,(mapconcat 'identity
                       (mapcar
                        (lambda (maildir)
                          (concat "maildir:" (car maildir)))
                        mu4e-maildir-shortcuts) " OR ")
           "All maildirs with shortcuts" ?a)))

  (prefer-coding-system 'utf-8)
  (setq system-time-locale "en_GB" )

  ;; get c-h working
  (set-keyboard-coding-system nil)
  (setq mac-pass-command-to-system nil)

  ;; enable autocomplete
  (auto-complete-mode t)

  ;; Another vairable
  (setq helm-echo-input-in-header-line nil)

  ;; Don’t ask me when close emacs with process is running
  (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
    "Prevent annoying \"Active processes exist\" query when you quit Emacs."
    (flet ((process-list ())) ad-do-it))

  ;; Don’t ask me when kill process buffer
  (setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
              kill-buffer-query-functions))

 (defun snaptrip-start ()
   "Start all the right processes for snaptrip"
   (interactive)
   (let (
         (default-directory (cd-absolute "~/snaptrip"))
         (original-buffer (buffer-name))
         (redis-buffer (generate-new-buffer-name "### redis"))
         (server-buffer (generate-new-buffer-name "### server"))
          (elasticsearch-buffer (generate-new-buffer-name "### elasticsearch"))
          )
     (eshell redis-buffer)
     (set-buffer redis-buffer)
     (insert "redis-server")
     (comint-send-input)
     ;;
     (eshell console-buffer)
     (set-buffer console-buffer)
     (insert "be rails c")
     (comint-send-input)
     ;;
     (eshell server-buffer)
     (set-buffer server-buffer)
     (insert "be rails server")
     (comint-send-input)
     ;;
     (eshell elasticsearch-buffer)
     (set-buffer elasticsearch-buffer)
     (insert "elasticsearch --config=/usr/local/opt/elasticsearch/config/elasticsearch.yml")
     (comint-send-input)
     ;;
     (echo "Snaptrip's booting up! :D")
     (switch-to-buffer original-buffer)
     ))

  (define-key evil-insert-state-map "\C-e" 'end-of-line)
  (define-key evil-visual-state-map "\C-e" 'end-of-line)
  (define-key evil-normal-state-map "\C-e" 'end-of-line)
  (define-key evil-normal-state-map "Y" "y$")


  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; (setq tab-width 2)

  (global-set-key (kbd "s-3") `(lambda () (interactive) (insert "#")))

  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(custom-safe-themes
   (quote
    ("62408b3adcd05f887b6357e5bd9221652984a389e9b015f87bbc596aba62ba48" default)))
 '(fancy-battery-mode t)
 '(ring-bell-function (quote ignore) t)
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
