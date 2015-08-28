;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.


(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers

   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (ruby :variables
           ruby-version-manager 'rbenv
           ruby-enable-ruby-on-rails-support t)
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
            shell-default-shell 'shell
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

     mu4e

     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(soft-charcoal-theme
                                      color-theme-sanityinc-tomorrow
                                      twittering-mode
                                      sx
                                      gnugo
                                      evil-rails
                                      w3m
                                      ;; auth-password-store

                                      )

   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.

  (setq-default
   ;; for ruby-version-manager
   ruby-version-manager 'rbenv
   ruby-enable-ruby-on-rails-support t
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
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
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
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
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; Default value is `cache'.
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f) is replaced.
   dotspacemacs-use-ido nil
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state nil
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup t
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native t
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible value is `all',
   ;; `current' or `nil'. Default is `all'
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server t
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "ack" "grep")
   dotspacemacs-default-package-repository nil
)
  ;; User initialization goes here
  )

(defun dotspacemacs/config ()

  ;; Mac
  (setq vc-follow-symlinks t)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)


  ;; Twitter
  (setq twittering-use-master-password t)

  ;; Go
  (evil-declare-key 'motion gnugo-board-mode-map (kbd "<return>") 'gnugo-move)
  (evil-declare-key 'motion gnugo-board-mode-map (kbd "RET") 'gnugo-move)
  (evil-declare-key 'motion gnugo-board-mode-map (kbd "q") 'gnugo-quit)

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
          (original-buffer (buffer-name))
          (redis-buffer (generate-new-buffer-name "### redis"))
          (zeus-buffer (generate-new-buffer-name "### zeus"))
          (server-buffer (generate-new-buffer-name "### server"))
          (worker-buffer (generate-new-buffer-name "### worker"))
          (elasticsearch-buffer (generate-new-buffer-name "### elasticsearch"))
          )
      (shell redis-buffer)
      (set-buffer redis-buffer)
      (insert "cd ~/Desktop/snap-trip")
      (comint-send-input)
      (insert "redis-server")
      (comint-send-input)

      (shell zeus-buffer)
      (set-buffer zeus-buffer)
      (insert "cd ~/Desktop/snap-trip")
      (comint-send-input)
      (insert "zeus start")
      (comint-send-input)

      (shell server-buffer)
      (set-buffer server-buffer)
      (insert "cd ~/Desktop/snap-trip")
      (comint-send-input)
      (insert "sleep 5")
      (comint-send-input)
      (insert "zeus server")
      (comint-send-input)

      (shell worker-buffer)
      (set-buffer worker-buffer)
      (insert "cd ~/Desktop/snap-trip")
      (comint-send-input)
      (insert "sleep 5")
      (comint-send-input)
      (insert "zeus rake resque:work")
      (comint-send-input)

      (shell elasticsearch-buffer)
      (set-buffer elasticsearch-buffer)
      (insert "cd ~/Desktop/snap-trip")
      (comint-send-input)
      (insert "elasticsearch --config=/usr/local/opt/elasticsearch/config/elasticsearch.yml")
      (comint-send-input)

      (echo "Snaptrip's booting up! :D")
      (switch-to-buffer original-buffer)
      ))

      (define-key evil-insert-state-map "\C-e" 'end-of-line)
      (define-key evil-visual-state-map "\C-e" 'end-of-line)
      (define-key evil-normal-state-map "\C-e" 'end-of-line)

      (add-hook 'before-save-hook 'delete-trailing-whitespace)

      (setq tab-width 2)

      ;; (define-key window-numbering-keymap "\M-0" nil)
      ;; (define-key window-numbering-keymap "\M-1" nil)
      ;; (define-key window-numbering-keymap "\M-2" nil)
      ;; (define-key window-numbering-keymap "\M-3" nil)
      ;; (define-key window-numbering-keymap "\M-4" nil)
      ;; (define-key window-numbering-keymap "\M-5" nil)
      ;; (define-key window-numbering-keymap "\M-6" nil)
      ;; (define-key window-numbering-keymap "\M-7" nil)
      ;; (define-key window-numbering-keymap "\M-8" nil)
      ;; (define-key window-numbering-keymap "\M-9" nil)
                                              ; Actually allow typing #
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
