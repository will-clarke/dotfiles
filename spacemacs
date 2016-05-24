;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     yaml
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; auto-completion
     better-defaults
     emacs-lisp
     (ruby :variables
           ruby-version-manager 'rbenv
           ruby-insert-encoding-magic-comment nil
           enh-ruby-add-encoding-comment-on-save nil)
     html
     markdown
     sql
     org
     (mu4e :variables
           mu4e-enable-mode-line t
           mu4e-enable-notifications t)
     (git :variables
          git-magit-status-fullscreen t
          git-enable-github-support t
          git-gutter-use-fringe t)
     (shell :variables
            shell-default-term-shell "/bin/bash"
            shell-default-shell 'eshell
            shell-default-position 'bottom
            shell-default-height 50)

     version-control
     dash
     github
     emoji
     (spell-checking :variables spell-checking-enable-by-default nil)
     syntax-checking
     c-c++
     spotify
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence "jk"
                      auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-private-snippets-directory "~/.spacemacs.d/snippets/")
   ruby-on-rails
   restclient
   )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.)
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-light
                         solarized-dark)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 23
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)

   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native t
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'current
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put almost
any user code here.  The exception is org related code, which should be placed
in `dotspacemacs/user-config'."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

  (with-eval-after-load 'mu4e-alert
    ;; Enable Desktop notifications
    ;; (mu4e-alert-set-default-style 'notifications)) ; For linux
  ;; (mu4e-alert-set-default-style 'libnotify))  ; Alternative for linux
  ;; (mu4e-alert-set-default-style 'notifier))   ; For Mac OSX (through the
                                        ; terminal notifier app)
  (mu4e-alert-set-default-style 'growl))      ; Alternative for Mac OSX

  (display-time-mode 1)

  (defun source (filename &optional use_default_gpg_key)
    "Update environment variables from a shell source file."
    (interactive "fSource file: ")
    (message "Sourcing environment from `%s'..." filename)
    (with-temp-buffer
      (if use_default_gpg_key
          (shell-command (format "diff -u <(true; export) <(eval $((gpg --passphrase \"`security find-generic-password -a megalolz -s megalolz -w`\"  -d ~/.secrets.gpg) 2> /dev/null); export)" ) '(4))
        (shell-command (format "diff -u <(true; export) <(source %s; export)" filename) '(4))
        )
      (let ((envvar-re "declare -x \\([^=]+\\)=\\(.*\\)$"))
        (while (search-forward-regexp (concat "^-" envvar-re) nil t)
          (let ((var (match-string 1)))
            (message "%s" (prin1-to-string `(setenv ,var nil)))
            (setenv var nil)))
        (goto-char (point-min))
        (while (search-forward-regexp (concat "^+" envvar-re) nil t)
          (let ((var (match-string 1))
                (value (read (match-string 2))))
            (message "%s" (prin1-to-string `(setenv ,var ,value)))
            (setenv var value)))))
    (message "Sourcing environment from `%s'... done." filename))
  (source "~/.secrets.gpg" t)









;;  ;; https://www.emacswiki.org/emacs/CompileCommand
;;  (add-hook 'c-mode-hook
;;            (lambda ()
;;              (unless (file-exists-p "Makefile")
;;                (set (make-local-variable 'compile-command)
;;                     ;; emulate make's .c.o implicit pattern rule, but with
;;                     ;; different defaults for the CC, CPPFLAGS, and CFLAGS
;;                     ;; variables:
;;                     ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
;;                     (let ((file (file-name-nondirectory buffer-file-name)))
;;                       ;; (format "%s -c -o %s.o %s %s %s"
;;                       (format "%s %s -o %s.o %s %s && ./%s.o"
;;                               (or (getenv "CC") "gcc")
;;                               file
;;                               (file-name-sans-extension file)
;;                               (or (getenv "CPPFLAGS") "-DDEBUG=9")
;;                               (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
;;                               (file-name-sans-extension file)
;;                               ))))))





 ;;  (setq org-agenda-files (list "~/todo.org"
 ;;                               "~/Dropbox/Dev/org-mode/work.org"))

  ;; Mac
  (setq vc-follow-symlinks t)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)

  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
  (set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))
  ;; example:  C-x C-f /sudo:root@host[#port]:/path/to/file
  (require 'tramp)
  (defun sudo-edit-current-file ()
    (interactive)
    (let ((position (point)))
      (find-alternate-file
       (if (file-remote-p (buffer-file-name))
           (let ((vec (tramp-dissect-file-name (buffer-file-name))))
             (tramp-make-tramp-file-name
              "sudo"
              (tramp-file-name-user vec)
              (tramp-file-name-host vec)
              (tramp-file-name-localname vec)))
         (concat "/sudo:root@localhost:" (buffer-file-name))))
      (goto-char position)))



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
  ;; (mu4e/mail-account-reset)
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

 ;;  ;; get c-h working
 ;;  (set-keyboard-coding-system nil)
 ;;  (setq mac-pass-command-to-system nil)

 ;;  ;; Don’t ask me when close emacs with process is running
 ;;  (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
 ;;    "Prevent annoying \"Active processes exist\" query when you quit Emacs."
 ;;    (flet ((process-list ())) ad-do-it))

  (setq eshell-aliases-file (concat dotspacemacs-directory "eshell/alias"))

  (defun make-eshell-named (name)
    "Create a shell buffer named NAME."
    (eshell)
    (rename-buffer name))

 (defun snaptrip_start ()
   "Start all the right processes for snaptrip"
   (interactive)
   (require 'linum)
   (save-excursion
   (let (
         (original-buffer (buffer-name))
         (zeus-buffer (make-eshell-named "### zeus"))
         (redis-buffer (make-eshell-named "### redis"))
         (elasticsearch-buffer (make-eshell-named "### elasticsearch"))
         (server-buffer (make-eshell-named "### server"))
         (worker-buffer (make-eshell-named "### worker"))
         (console-buffer (make-eshell-named "### console"))
         )
     ;;
     (set-buffer zeus-buffer)
     (eshell/cd (directory-file-name "~/snaptrip"))
     (insert "zeus start")
     (eshell-send-input)
     ;;
     (set-buffer elasticsearch-buffer)
     (eshell/cd (directory-file-name "~/snaptrip"))
     ;; (insert "elasticsearch --config=/usr/local/opt/elasticsearch/config/elasticsearch.yml")
     (insert "elasticsearch")
     (eshell-send-input)
     ;;
     (set-buffer redis-buffer)
     (eshell/cd (directory-file-name "~/snaptrip"))
     (insert "redis-server")
     (eshell-send-input)
     ;;
     (sleep-for 3)
     ;;
     (set-buffer server-buffer)
     (eshell/cd (directory-file-name "~/snaptrip"))
     (insert "zeus server")
     (eshell-send-input)
     ;;
     (set-buffer worker-buffer)
     (eshell/cd (directory-file-name "~/snaptrip"))
     (insert "zeus rake resque:work")
     (eshell-send-input)
     ;;
     (set-buffer console-buffer)
     (eshell/cd (directory-file-name "~/snaptrip"))
     (insert "zeus console")
     (eshell-send-input)
     ;;
     (message "Snaptrip's booting up! :D")
     (switch-to-buffer original-buffer)
     )))


 ;; ;; dired has a groovy way of going up directories
 (evil-declare-key 'normal dired-mode-map (kbd ";") 'dired-up-directory)

 (define-key evil-insert-state-map (kbd "s-3") `(lambda () (interactive) (insert "#")))

 (define-key evil-insert-state-map "\C-e" 'end-of-line)
 (define-key evil-visual-state-map "\C-e" 'end-of-line)
 (define-key evil-normal-state-map "\C-e" 'end-of-line)

 (setq kill-buffer-query-functions
       (remq 'process-kill-buffer-query-function
             kill-buffer-query-functions))

 (defun my-require-pry ()
   (interactive)
   (save-excursion
     (evil-insert-newline-above)
     (indent-according-to-mode)
     (insert "require 'pry'")
     (evil-insert-newline-below)
     (indent-according-to-mode)
     (insert "binding.pry")))

 (evil-leader/set-key "or" 'my-require-pry)

 (evil-leader/set-key "wo"  'other-window)
 (evil-leader/set-key "ow"  'other-window)


 (evil-leader/set-key "op" '(lambda ()
                              (interactive)
                              (inf-ruby-switch-from-compilation)
                              (end-of-line)
                              (insert)))


 (global-set-key (kbd "s-v") 'yank)
 (global-set-key (kbd "M-v") 'yank)
 (global-set-key (kbd "M-c") 'evil-yank)
 (global-set-key (kbd "M-a") 'mark-whole-buffer)

 (setq org-mu4e-convert-to-html t)
 ;; Set to the location of your Org files on your local system
 (setq org-directory "~/org")
 ;; Set to the name of the file where new notes will be stored
 (setq org-mobile-inbox-for-pull "~/org/flagged.org")
 ;; Set to <your Dropbox root directory>/MobileOrg.
 (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

 ;; postgres:
 ;; To start:  mx: sql-connect -> olive
 ;; OR:        sql-postgres
 ;;            Put in login detils
 ;;              New buffer: whatever.sql
 ;;              Write query. Send it to original SQLi[Postgres] buffer.
 ;;            Remember the ';' :|

 ;; (setq sql-postgres-login-params
 ;;       '((user :default (getenv "ST_PG_USER"))
 ;;         (database :default (getenv "ST_PG_DB"))
 ;;         (server :default (getenv "ST_PG_SERVER"))
 ;;         (password :default (getenv "ST_PG_PW"))
 ;;         (port :default 5662)))

 )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(sql-connection-alist
   (quote
    (("olive"
      (sql-product
       (quote postgres))
      (sql-user (getenv "ST_PG_USER"))
      (sql-database (getenv "ST_PG_DB"))
      (sql-server (getenv "ST_PG_SERVER"))
      (sql-password (getenv "ST_PG_PW"))
      (sql-port 5662)))))
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
