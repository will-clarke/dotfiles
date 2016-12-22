;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-return-key-behavior 'nil
                      auto-completion-tab-key-behavior 'complete
                      ;; auto-completion-tab-key-behavior 'cycle
                      ;; auto-completion-complete-with-key-sequence nil
                      ;; auto-completion-complete-with-key-sequence-delay 0.1
                      ;; auto-completion-private-snippets-directory nil)
                      )
     vimscript
     yaml
     ivy
     javascript
     yaml
     erlang
     ;; xkcd
     ;; gtags
     better-defaults
     ;; plantuml
     emacs-lisp
     ;; common-lisp
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
            ;; shell-default-shell 'multi-term
            shell-enable-smart-eshell t
            shell-default-position 'bottom
            shell-default-height 30)
     version-control
     dash
     github
     emoji
     ;; (spell-checking :variables spell-checking-enable-by-default nil)
     syntax-checking
     c-c++
     ruby-on-rails
     restclient
     haskell
     ;; deft
     ;; pdf-tools
     ;; elixir
     )

   ;; dotspacemacs-additional-packages '(
   ;;                                    org-alert
   ;;                                    helm-w3m
   ;;                                    org-mac-link
   ;;                                    soft-charcoal-theme
   ;;                                    )
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(evil-unimpaired smartparens)
   dotspacemacs-delete-orphan-packages t
   dotspacemacs-download-packages 'used))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-scratch-mode 'text-mode
   ;; dotspacemacs-themes '(spacemacs-dark
   ;;                       spacemacs-light)
  dotspacemacs-themes '(solarized-light
                        soft-charcoal)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 20
                               ;; :size 13
                               ;; :size 15
                               :weight normal
                               :width normal
                               :powerline-scale 1.5)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-transient-state t
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native t
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'nil
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("ag" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()

  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "https://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.org/packages/")))


  ;;; W3M
  ;; (require 'helm-w3m)
  ;; (evil-leader/set-key "os" 'w3m-search)
  ;; (setq w3m-home-page "http://www.google.com")
  ;; ;; W3M Home Page
  ;; (setq w3m-default-display-inline-images t)
  ;; (setq w3m-default-toggle-inline-images t)
  ;; ;; W3M default display images
  ;; (setq w3m-command-arguments '("-cookie" "-F"))
  ;; (setq w3m-use-cookies t)
  ;; ;; W3M use cookies
  ;; ;; (setq browse-url-browser-function 'w3m-browse-url)
  ;; ;; Browse url function use w3m
  ;; (setq w3m-view-this-url-new-session-in-background t)
  ;; ;; W3M view url new session in background
  ;; (evil-declare-key 'normal w3m-mode-map (kbd "RET") 'w3m-view-this-url)


  ;; MU4E
  ;; (with-eval-after-load 'mu4e-alert
  ;; Enable Desktop notifications
  ;; (mu4e-alert-set-default-style 'notifications)) ; For linux
  ;; (mu4e-alert-set-default-style 'libnotify))  ; Alternative for linux
  ;; (mu4e-alert-set-default-style 'notifier))   ; For Mac OSX (through the ; terminal notifier app)
  ;; (mu4e-alert-set-default-style 'growl))      ; Alternative for Mac OSX
  ;; (mu4e-alert-enable-notifications)
  ;; mu4e-alert-enable-mode-line-display
  (setq magit-emacsclient-executable "/usr/local/bin/emacsclient")
  (setq mu4e-account-alist
        '(("gmail"
           (mu4e-sent-messages-behavior delete)
           (mu4e-sent-folder "/gmail/Sent")
           (mu4e-refile-folder  "/gmail/Archive")
           (mu4e-trash-folder  "/gmail/Trash")
           (mu4e-follow-up-folder  "/gmail/Later")
           (mu4e-drafts-folder "/gmail/Drafts")
           (user-mail-address "wmmclarke@gmail.com")
           (user-full-name "William"))))
  ;; (mu4e/mail-account-reset)
  (setq
   mu4e-maildir "~/.mail"
   mu4e-trash-folder "/trash"
   mu4e-refile-folder "/archive"
   mu4e-get-mail-command "mbsync Inboxes; mbsync -a"
   ;; 900 second = 15 minutes
   ;; 300 second = 5 minutes
   mu4e-update-interval 300
   mu4e-hide-index-messages t
   mu4e-index-update-in-background t
   mu4e-compose-signature-auto-include nil
   mu4e-view-show-images t
   mu4e-view-show-addresses t
   mu4e-html2text-command "w3m -dump -T text/html"
   mu4e-view-prefer-html t
   mu4e-use-fancy-chars t
   mu4e-split-view 'vertical
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
          ("/gmail/Later" . ?l)))
  (setq mu4e-bookmarks
        `(
          ("NOT maildir:/archive AND NOT maildir:'/[Gmail].All Mail' AND date:today" "Unread Today" ?b)
          ("maildir:/gmail/Inbox" "All Inboxes" ?i)
          ("maildir:/gmail/Later" "All Later" ?l)
          ("date:today..now" "Today's messages" ?t)
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
  (setq mu4e-mu-binary "/usr/local/bin/mu")
  (setq mu4e-refile-folder "/archive")

  ;; OTHER
  (display-time-mode 1)
  ;; https://www.emacswiki.org/emacs/CompileCommand
  ;; (add-hook 'c-mode-hook
  ;;           (lambda ()
  ;;             (unless (file-exists-p "Makefile")
  ;;               (set (make-local-variable 'compile-command)
  ;;                    (let ((file (file-name-nondirectory buffer-file-name)))
  ;;                      (format "%s %s -o %s.o %s %s && ./%s.o"
  ;;                              (or (getenv "CC") "gcc")
  ;;                              file
  ;;                              (file-name-sans-extension file)
  ;;                              (or (getenv "CPPFLAGS") "-DDEBUG=9")
  ;;                              (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
  ;;                              (file-name-sans-extension file)))))))
  (setq projectile-tags-command "ctags -Re -f \"%s\" %s --exclude=*.html --exclude=*.js")
  (prefer-coding-system 'utf-8)
  (setq system-time-locale "en_GB" )
  ;; Kill Buffers without asking  - even if they have a process
  (setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
              kill-buffer-query-functions))
  (setq evil-search-module 'evil-search)
  ;; Read logs continuously
  (add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))
  (setq deft-directory "~/Dropbox/notes")
  (setq paradox-github-token (getenv "PARADOX_GITHUB_TOKEN"))
  ;; How to use ag:
  ;; ag: -G\.js something
  (setq disaster-objdump "gobjdump -d -M intel -Sl --no-show-raw-insn")
  (setq inferior-lisp-program "/Users/wmmc/.nix-profile/bin/sbcl")
  (setq mu4e-mu-binary "/usr/local/bin/mu")

  ;; ESHELL
  (setq eshell-aliases-file (concat dotspacemacs-directory "eshell/alias"))
  (setq eshell-buffer-shorthand nil) ;; for annoying error with $(uname) shell dollar expansion
  (defun make-eshell-named (name)
    "Create a shell buffer named NAME."
    (eshell)
    (rename-buffer name))

  ;; My Keymaps
  (evil-declare-key 'normal dired-mode-map (kbd ";") 'dired-up-directory)
  (define-key evil-insert-state-map (kbd "s-3") `(lambda () (interactive) (insert "#")))
  (define-key evil-insert-state-map "\C-e" 'end-of-line)
  (define-key evil-visual-state-map "\C-e" 'end-of-line)
  (define-key evil-normal-state-map "\C-e" 'end-of-line)
  (evil-leader/set-key "om" 'my_mu4e_buffer)
  (evil-leader/set-key "wo"  'other-window)
  (evil-leader/set-key "ow"  'other-window)
  (evil-leader/set-key "ol"  'link-hint-open-link-at-point)
  (evil-leader/set-key "oc" 'my/w3m-chrome)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "M-v") 'yank)
  (global-set-key (kbd "M-c") 'evil-yank)
  (global-set-key (kbd "M-a") 'mark-whole-buffer)
  (evil-leader/set-key "or" 'my-require-pry)
  (global-set-key (kbd "s-=") 'spacemacs/scale-up-font)
  (global-set-key (kbd "s--") 'spacemacs/scale-down-font)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (evil-leader/set-key "op" '(lambda ()
                               (interactive)
                               (inf-ruby-switch-from-compilation)
                               (end-of-line)
                               (insert)))
  (evil-leader/set-key (kbd "1") 'my/get-server)
  (evil-leader/set-key (kbd "2") 'my/get-worker)
  (define-key evil-normal-state-map "\C-]" 'helm-gtags-find-tag)
  (evil-define-key 'normal evil-c-mode-map
    "\C-[" 'helm-gtags-find-tag)
  (evil-define-key 'insert evil-term-mode-map
    "M-DEL" 'term-send-backward-kill-word)
  (evil-define-key 'insert term-raw-map
    "M-DEL" 'term-send-backward-kill-word)

  (global-set-key (kbd "s-j") 'scroll-other-window)
  (global-set-key (kbd "s-k") 'scroll-other-window-down)

  (global-set-key (kbd "C-M-H") 'backward-kill-word)
  (global-set-key (kbd "M-h") 'backward-kill-word)

  ;; Tags
  ;; ctags --force-language=ruby -R -u
  ;; ctags -R --language-force=ruby --exclude=.git --exclude=log . $(bundle list --paths)
  ;; (define-key evil-motion-state-map "\C-]" 'helm-gtags-find-tag-from-here);; evil-jump-to-tag)
  ;; (spacemacs/helm-gtags-define-keys-for-mode 'ruby-mode)


  ;; (require 're-builder)
  (setq reb-re-syntax 'string)

  ;; My Functions
  (defun my-require-pry ()
    (interactive)
    (save-excursion
      (evil-insert-newline-above)
      (indent-according-to-mode)
      (insert "require 'pry'")
      (evil-insert-newline-below)
      (indent-according-to-mode)
      (insert "binding.pry")))
  (defun my_mu4e_buffer ()
    "Navigate to my mu4e buffer"
    (interactive)
    (save-excursion
      (mu4e-headers-search (concat "NOT maildir:"
                                   "\"/archive\" "
                                   "AND NOT maildir:"
                                   "\"/[Gmail].All Mail\" "
                                   "AND date:today"))
      ))
  (defun my/current-chrome-url ()
    (interactive)
    (do-applescript
     (concat
      "set frontmostApplication to path to frontmost application\n"
      "tell application \"Google Chrome\"\n"
      "	set theUrl to get URL of active tab of first window\n"
      "	set theResult to (get theUrl)\n"
      "end tell\n"
      "return theResult as string\n"
      )
     )
    )
  ;; (defun my/w3m-chrome ()
  ;;   "Open w3m with whatever's in Chrome"
  ;;   (interactive)
  ;;   (w3m-goto-url (my/current-chrome-url))
  ;;   )
  (defun my/which-major-mode()
    "Displays the major mode"
    (interactive)
    (message "%s" major-mode)
    )

  (defun my/get-server()
    (interactive)
    (switch-to-buffer(get-buffer "### server")))
  (defun my/get-worker()
    (interactive)
    (switch-to-buffer(get-buffer "### worker")))

  ;; Postgres
  ;; To start:  mx: sql-connect -> olive
  ;; OR:        sql-postgres
  ;;            Put in login detils
  ;;              New buffer: whatever.sql
  ;;              Write query. Send it to original SQLi[Postgres] buffer.
  ;;            Remember the ';' :|
  (setq sql-connection-history nil)
  ;; set search_path to cd;
  (setq sql-connection-alist
        '(
          ("domains"
           (sql-product 'postgres)
           (sql-server (getenv "DB_HOST"))
           (sql-user "wmmclarke")
           (sql-database "domainsdb")
           (sql-password (getenv "DB_PASSWORD"))
           (sql-port 5432))
          )
        )


  ;; probs delete
  ;; (require 'eshell)
  ;; (require 'em-smart)
  ;; (setq eshell-where-to-jump 'begin)
  ;; (setq eshell-review-quick-commands nil)
  ;; (setq eshell-smart-space-goes-to-end t)

  ;; Smartparens
  ;; https://github.com/Fuco1/smartparens/wiki/Permissions
  (sp-pair "'" nil :unless '(sp-point-before-word-p))
  (sp-pair "\"" nil :unless '(sp-point-before-word-p))
  (sp-pair "[" nil :unless '(sp-point-before-word-p))
  (sp-pair "{" nil :unless '(sp-point-before-word-p))
  ;; (sp-pair "'" nil :unless '(sp-point-before-symbol-p))
  ;; (sp-pair "\"" nil :unless '(sp-point-before-symbol-p))
  ;; (sp-pair "[" nil :unless '(sp-point-before-symbol-p))
  ;; (sp-pair "{" nil :unless '(sp-point-before-symbol-p))

  ;; GPG
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
  (source "~/.variables")
  (defun wg/kludge-gpg-agent
      ()
    (if
        (display-graphic-p)
        (setenv "DISPLAY"
                (terminal-name))
      (setenv "GPG_TTY"
              (terminal-name))
      (setenv "DISPLAY")))
  (add-hook 'window-configuration-change-hook 'wg/kludge-gpg-agent)

  ;; Mac
  (setq vc-follow-symlinks t)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)

  ;; Tramp / SSH
  ;; normal tramp use: SPC-F-F `/ssh:pi:` or /ssh:user@ip:/directory/file.txt
  ;;   uses ~/.ssh/config
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

  ;; ORG
  ;; (require 'org-mac-link)
  (setq org-agenda-files (list "~/org"))
  ;; "~/Dropbox/Dev/org-mode/work.org"
  (setq org-agenda-include-diary t)
  ;; (global-set-key (kbd "C-`") 'ort/goto-todos)
;;   (setq alert-default-style 'growl)
;;   (setq org-mu4e-link-query-in-headers-mode nil)
;;   ;; Set to the location of your Org files on your local system
;;   (setq org-directory "~/org")
;;   ;; Set to the name of the file where new notes will be stored
;;   (setq org-mobile-inbox-for-pull "~/org/uploaded.org")
;;   ;; Set to <your Dropbox root directory>/MobileOrg.
;;   (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
;;   (setq org-agenda-files (list "~/org/todo.org"))
;;   (setq org-bullets-bullet-list '("⚫" "◉" "○" "►" "◎" "◇"))

;;   ;; babel
  (org-babel-do-load-languages
   'org-babel-load-languages '((C . t)
                               (plantuml . t)
                               (ruby . t)
                               (sh . t)
                               ))

  ;; spaceline / powerline / bar at bottom
  (spaceline-toggle-minor-modes-off)
  (setq powerline-default-separator 'alternate)

  ;; (setq which-key-paging-prefixes '("SPC"))
  ;; ;; (define-key which-key-mode-map (kbd "C-x <right>") 'which-key-C-h-dispatch)
  ;; (setq which-key-paging-key "<right>")

;;   (defvar my/org-basic-task-template "* TODO %^{Task}
;; :PROPERTIES:
;; :Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
;; :END:
;; Captured %<%Y-%m-%d %H:%M>
;; %?

;; %i
;; " "Basic task data")

;;   (setq org-capture-templates '(
;;                                 ("1" "~/org/todo.org #Tasks" entry
;;                                  (file+headline "~/org/todo.org" "Tasks")
;;                                  "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
;;                                 ("t" "Task" entry
;;                                  (file "~/org/refile.org")
;;                                  "* TODO %?\n")
;;                                 ("T" "Clock-in Task" entry
;;                                  (file "~/org/refile.org")
;;                                  "* TODO %?\n"
;;                                  :clock-in t
;;                                  :clock-resume t)
;;                                 ("d" "Distraction in a pomodoro" entry
;;                                  (file "~/org/refile.org")
;;                                  "* TODO %^{Task}\n  SCHEDULED: %t\n"
;;                                  :immediate-finish t)
;;                                 ("n" "Note" entry
;;                                  (file "~/org/refile.org")
;;                                  "* %?\n")
;;                                 ("l" "Note with link to current file" entry
;;                                  (file "~/org/refile.org")
;;                                  "* %a")
;;                                 ("c" "Link from Chrome" entry
;;                                  (file "~/org/refile.org")
;;                                  "* %(org-mac-chrome-get-frontmost-url)")
;;                                 ("C" "Clock-in Link from Chrome" entry
;;                                  (file "~/org/refile.org")
;;                                  "* %(org-mac-chrome-get-frontmost-url)"
;;                                  :clock-in t
;;                                  :clock-resume t)
;;                                 ))
;;   (require 'org-mu4e)
;;   (setq org-mu4e-convert-to-html t)
;;   ;; mu4e org: M-x org~mu4e-mime-switch-headers-or-body
;;   ;; https://github.com/djcb/mu/pull/196#issuecomment-36305657
;;   (defun org-export-string-hack (string backend &optional body-only ext-plist)
;;     (org-export-string-as (concat "#+OPTIONS: tex:dvipng toc:nil
;; " string) 'html t))
;;   (defalias 'org-export-string 'org-export-string-hack)
;;   (setq org-refile-targets '((nil :maxlevel . 2)
;;                              (org-agenda-files :maxlevel . 2)))
;;   (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
;;   (setq org-refile-use-outline-path t)                  ; Show full paths for refiling
;;   (setq org-clone-delete-id t)
;;   (setq org-export-with-sub-superscripts nil)
;;   (setq org-export-with-toc nil)
;;   (setq org-export-with-section-numbers nil)
;;   (setq org-html-table-default-attributes '(:align "|c|c|c|" :border "3" :rules "all" :frame "border" :cellpadding "8"))
;;   ;; (setq org-html-table-default-attributes
;;   ;;       '(:class "table table-striped table-bordered table-condensed"
;;   ;;                :style "width: auto;"))
;;   ;; (setq org-html-table-default-attributes
;;   ;;       '(:border "0" :cellspacing "0" :cellpadding "6" :rules "none" :frame "none"))


;;   (global-prettify-symbols-mode)

;;   ;; UTF-8 please
;;   (setq locale-coding-system    'utf-8)
;;   (set-terminal-coding-system   'utf-8)
;;   (set-keyboard-coding-system   'utf-8)
;;   (set-selection-coding-system  'utf-8)
;;   (prefer-coding-system         'utf-8)
;;   (set-language-environment     'utf-8)

;;   ;; pdftools:
;;   (setenv "PKG_CONFIG_PATH" "/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig")

;;   ;; plantuml
;;   (setq puml-plantuml-jar-path "/usr/local/Cellar/plantuml/8041/plantuml.8041.jar")
;;   (setq plantuml-jar-path "/usr/local/Cellar/plantuml/8041/plantuml.8041.jar")
;;   (setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/8041/plantuml.8041.jar")

;;   ;; ivy:
;; C-M-j -> complete whatever word you were typing
;;   ;; console-M-x -> fuzzy searching
;;   (setq ivy-re-builders-alist
;;         '((t . ivy--regex-fuzzy)))

;;   (setq org-startup-folded nil)


  ;; restclient mode: no localhost -> use 127.0.0.1
  ;;     eg. POST http://127.0.0.1:3000/visitor_emails
  ;;     CHECK SERVER IS ACTUALLY WORKING!! DUH!

  ;; \C-h is now DEL
  (define-key key-translation-map [?\C-h] [?\C-?])

  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(evil-want-Y-yank-to-eol t)
 '(package-selected-packages
   (quote
    (erlang company-quickhelp ob-restclient company-restclient know-your-http-well company-web web-completion-data company-tern tern company-statistics company-emoji company-cabal company-c-headers auto-yasnippet ac-ispell auto-complete intero hlint-refactor hindent haskell-snippets flycheck-haskell company-ghci company-ghc ghc company haskell-mode cmm-mode powerline request rake pcre2el spinner org alert log4e gntp markdown-mode skewer-mode simple-httpd json-snatcher json-reformat hydra parent-mode haml-mode gitignore-mode fringe-helper git-gutter+ gh marshal logito pcache pos-tip flx magit-popup with-editor evil goto-chg highlight f s diminish projectile pkg-info epl popup bind-map bind-key async avy package-build vimrc-mode dactyl-mode mwim macrostep elisp-slime-nav auto-compile packed inflections yasnippet multiple-cursors ivy-purpose window-purpose imenu-list hide-comnt anzu iedit smartparens undo-tree flycheck git-gutter helm helm-core ht magit git-commit inf-ruby js2-mode yaml-mode wgrep smex ivy-hydra counsel-projectile counsel-dash dash-functional counsel swiper ivy xterm-color ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tagedit sql-indent spacemacs-theme spaceline solarized-theme soft-charcoal-theme smeargle slim-mode shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restclient restart-emacs rbenv rainbow-delimiters quelpa pug-mode projectile-rails popwin persp-mode paradox orgit org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file ob-http neotree multi-term mu4e-maildirs-extension mu4e-alert move-text mmm-mode minitest markdown-toc magit-gitflow magit-gh-pulls lorem-ipsum livid-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-ag google-translate golden-ratio gnuplot github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md flycheck-pos-tip flx-ido fill-column-indicator feature-mode fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help emoji-cheat-sheet-plus emmet-mode dumb-jump disaster diff-hl define-word dash-at-point column-enforce-mode coffee-mode cmake-mode clean-aindent-mode clang-format chruby bundler auto-highlight-symbol aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
