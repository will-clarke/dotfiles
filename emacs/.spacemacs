;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; MY DOCS

;; template selector BEGIN_SRC block == <s + TAB

;; rg ripgrep:
;; (make sure it's actually rg :| )
;; How to filter by filename
;; `contact_reason -- -g=*md`

;; Vim surround: ysiw" (iw is a text object)

;; Ruby - rbenv playing up.
;; Try MX global-rbenv-mode

;; How to log into a db server on emacs:
;; M-X  sql-postgres


(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(graphviz
     python
     clojure
     elixir
     nixos
     (haskell :variables
              ;; haskell-completion-backend 'intero
              haskell-enable-hindent-style "johan-tibell"
              haskell-enable-hindent t)
     docker
     systemd
     neotree
     ranger
     (rust :variables rust-format-on-save t)
     (go :variables
         ;; go-use-gometalinter t
         go-format-before-save t
         godoc-at-point-function 'godoc-gogetdoc
         gofmt-command "goimports"
         )
     javascript
     ivy
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-return-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-private-snippets-directory "~/spacemacs.d/snippets")

     better-defaults
     (elm :variables
          elm-format-command "elm-format-0.17"
          )
     ;; elm-sort-imports-on-save t
     ;; elm-format-on-save t
     git
     markdown
     (org :variables
          ;; org-enable-trello-support t
          org-enable-hugo-support t
          org-want-todo-bindings t
          org-enable-github-support t
          org-enable-org-journal-support t
          org-journal-dir "~/org/journal/"
          org-journal-file-format "%Y-%m-%d"
          org-journal-date-prefix "#+TITLE: "
          org-journal-date-format "%A, %B %d %Y"
          org-journal-time-prefix "* "
          org-journal-time-format "")
     spacemacs-org
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-shell 'eshell
            )
     syntax-checking
     version-control
     (ruby :variables
           ruby-test-runner 'rspec)
     ruby-on-rails
     yaml
     sql
     github
     scala
     restclient
     html
     csv
     dash
     terraform
     (elfeed :variables
             elfeed-feeds '(
                            "https://feeds.feedburner.com/37signals/beMH"
                            "https://www.joelonsoftware.com/feed/"
                            "https://feeds.feedburner.com/codinghorror"
                            "https://feeds.feedburner.com/SteveysBlogRants"
                            "https://www.reddit.com/r/cryptocurrency/.rss"
                            "https://www.reddit.com/r/altcoins/.rss"
                            ))
     deft
     (erc :variables
          erc-server-list
          '(("irc.freenode.net"
             :port "6697"
             :ssl t
             :nick "some-user"
             :password "secret")
            ("irc.myworkirc.net"
             :port "1234"
             :nick "some-suit"
             :password "hunter2"))
      )
     ;; emoji
     emacs-lisp
     (spell-checking :variables
                     spell-checking-enable-by-default nil)
     multiple-cursors
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '()

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style '(vim :variables
                                    vim-style-remap-Y-to-y$ t)


   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 22
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state 't

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native t

   ;; If non-nil the frame is maximized when Emacs starts up.
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

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'current

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  ;; Scala stuff
  (push '("ensime" . "melpa-stable") package-pinned-packages)
  (push '("melpa-stable" . "stable.melpa.org/packages/") configuration-layer-elpa-archives)
  (setq-default flycheck-scalastylerc "/usr/local/etc/scalastyle_config.xml")

  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; ############ qqqqqqqqqqqq

  ;; (setq large-file-warning-threshold nil)
  ;; (setq tags-add-tables nil)

  ;; (require 'inf-haskell)
  ;; (require 'haskell-interactive-mode)
  ;; (define-key evil-insert-state-map (kbd "C-n" ) 'nil)
  ;; (define-key haskell-interactive-mode-map (kbd "C-n") #'haskell-interactive-mode-history-next)
  ;; (define-key haskell-interactive-mode-map (kbd "C-p") #'haskell-interactive-mode-history-previous)
  ;; (setq intero-whitelist '("~/Dropbox/dev/haskell/snake/app/Main.hs"))

  ;; How to log into a db server on emacs:
  ;; M-X  sql-postgres

  (setq sql-postgres-login-params
        '((user :default "postgres")
          (database :default "postgres")
          (server :default "localhost")
          (port :default 5432)))

  ;; https://truongtx.me/2014/08/23/setup-emacs-as-an-sql-database-client
  ;; (setq sql-connection-alist
  ;;       '((server1 (sql-product 'postgres)
  ;;                  (sql-port 5432)
  ;;                  (sql-server "localhost")
  ;;                  (sql-user "user")
  ;;                  (sql-password "password")
  ;;                  (sql-database "db1"))
  ;;         (server2 (sql-product 'postgres)
  ;;                  (sql-port 5432)
  ;;                  (sql-server "localhost")
  ;;                  (sql-user "user")
  ;;                  (sql-password "password")
  ;;                  (sql-database "db2"))))



  (require 'haskell-interactive-mode) (define-key evil-insert-state-map (kbd "C-k" ) 'nil) (define-key haskell-interactive-mode-map (kbd "C-j") #'haskell-interactive-mode-history-next) (define-key haskell-interactive-mode-map (kbd "C-k") #'haskell-interactive-mode-history-previous)

  (with-eval-after-load 'org
    (progn
      (setq org-agenda-files
            '("~/org"))
      (setq org-refile-targets
            '((nil :maxlevel . 2)
              (org-agenda-files :maxlevel . 2)))
      )
    (setq org-capture-templates
          '(("t" "TODO" entry (file+headline "~/org/todo.org" "Todo")
             "* TODO %? %^G \n  %U" :empty-lines 1)
            ("s" "Scheduled TODO" entry (file+headline "~/org/todo.org" "Todo")
             "* TODO %? %^G \nSCHEDULED: %^t\n  %U" :empty-lines 1)
            ("d" "Deadline" entry (file+headline "~/org/todo.org" "Todo")
             "* TODO %? %^G \n  DEADLINE: %^t" :empty-lines 1)
            ("w" "Work" entry (file+headline "~/org/work.org" "Work")
             "* TODO %? %^G\n%T")
            ("p" "Priority" entry (file+headline "~/org/todo.org" "Todo")
             "* TODO [#A] %? %^G \n  SCHEDULED: %^t")
            ("l" "Link" entry (file+headline "~/org/todo.org" "Todo")
             "* TODO %? %^G\n %a")
            ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
             "* %? %^G\n%U" :empty-lines 1)
            ("j" "Journal" entry (file+datetree "~/org/journal.org")
             "* %? %^G\nEntered on %U\n")))

    ;; Needs terminal-notifier (brew install terminal-notifier)
    (defun notify-osx (title message)
      (call-process "terminal-notifier"
                    nil 0 nil
                    "-group" "Emacs"
                    "-title" title
                    "-sender" "org.gnu.Emacs"
                    ;; -sound default
                    "-message" message))

    ;; org-pomodoro mode hooks
    (add-hook 'org-pomodoro-finished-hook
              (lambda ()
                (notify-osx "Pomodoro completed!" "Time for a break.")))

    (add-hook 'org-pomodoro-break-finished-hook
              (lambda ()
                (notify-osx "Pomodoro Short Break Finished" "Ready for Another?")))

    (add-hook 'org-pomodoro-long-break-finished-hook
              (lambda ()
                (notify-osx "Pomodoro Long Break Finished" "Ready for Another?")))

    (add-hook 'org-pomodoro-killed-hook
              (lambda ()
                (notify-osx "Pomodoro Killed" "One does not simply kill a pomodoro!")))

    )

  (autoload 'apib-mode "apib-mode"
    "Major mode for editing API Blueprint files" t)
  (add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode))

  (setq powerline-default-separator 'arrow) ;; potentially stop crashes..

  (setq erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (setq erc-lurker-threshold-time 3600)

  (setq ensime-startup-notification nil)

  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)

  (define-key key-translation-map (kbd "§") (kbd "#"))
  (define-key key-translation-map (kbd "s-3") (kbd "#"))

  (define-key evil-insert-state-map "\C-e" 'end-of-line)
  (define-key evil-visual-state-map "\C-e" 'end-of-line)
  (define-key evil-normal-state-map "\C-e" 'end-of-line)

  (evil-leader/set-key "ol"  'link-hint-open-link-at-point)

  (setq vc-follow-symlinks t)

  (defun wmmc/change-font-size (multiplier)
    "Change the font size globally."
    (set-face-attribute 'default nil :height
                        (floor (* multiplier
                                  (face-attribute 'default :height)))))

  (defun wmmc/increase-font-size ()
    "Increase font size globally."
    (interactive)
    (wmmc/change-font-size 1.10))

  (defun wmmc/decrease-font-size ()
    "Decrease font size globally."
    (interactive)
    (wmmc/change-font-size 0.9))

  (evil-leader/set-key "o+" 'wmmc/increase-font-size)
  (evil-leader/set-key "o=" 'wmmc/increase-font-size)
  (evil-leader/set-key "o-" 'wmmc/decrease-font-size)

  (eval-after-load 'elm-mode
    '(define-key evil-insert-state-map (kbd "<C-return>") 'newline))

  (eval-after-load "dired-mode"
    (evilified-state-evilify dired-mode dired-mode-map
      "G" 'end-of-buffer
      "gg" 'beginning-of-buffer
      ";" 'dired-up-directory))

  (eval-after-load 'rspec-mode
    '(define-key rspec-compilation-mode-map (kbd "C-c C-c")
       'inf-ruby-switch-from-compilation))

  (defun wmmc/find-next-file (&optional backward)
    (interactive "P")
    (when buffer-file-name
      (let* ((file (expand-file-name buffer-file-name))
             (files (cl-remove-if (lambda (file) (cl-first (file-attributes file)))
                                  (sort (directory-files (file-name-directory file) t nil t) 'string<)))
             (pos (mod (+ (cl-position file files :test 'equal) (if backward -1 1))
                       (length files))))
        (find-file (nth pos files)))))

  (evil-leader/set-key "on" 'wmmc/find-next-file)

  ;; to avoid wierd crashes when searching
  ;; (setq dotspacemacs-mode-line-unicode-symbols nil)
  (setq ediff-window-setup-function 'ediff-setup-windows-default)

  ;; Set PATH and exec-path
  (let*
      (
       (concatHome (lambda (path) (concat (getenv "HOME") path)))
       (mypaths
        (list
         (funcall concatHome "/bin")
         "/usr/local/bin"
         "/usr/local/MacGPG2/bin"
         (funcall concatHome "/.rbenv/shims")
         (funcall concatHome "/.local/bin")
         (funcall concatHome "/.cabal/bin")
         (funcall concatHome "/.nix-profile/bin")
         ) )
       )
    (setenv "PATH" (concat (mapconcat 'identity mypaths ":")  ":" (getenv "PATH")))
    (setq exec-path (append mypaths (list exec-directory) exec-path) )
    )
  (setenv "NIX_PATH" "nixpkgs=/Users/wmmc/.nix-defexpr/channels/nixpkgs:nixpkgs=/Users/wmmc/.nix-defexpr/channels/nixpkgs")

  ;; Solves error: ls does not support –dired; see `dired-use-ls-dired` for more details.
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))

  (defun wmmc/marked-markdown-preview ()
    "run Marked on the current file if Marked is installed;
  otherwise fallback to markdown-preview"
    (interactive)
    (let ((marked-app "/Applications/Marked\\ 2.app"))
      (if (file-exists-p "/Applications/Marked 2.app")
          (shell-command
           (format (concat "open -a " marked-app " %s")
                   (shell-quote-argument (buffer-file-name))))
        (markdown-preview))
      ))

  (eval-after-load 'markdown-mode
    '(progn
       (define-key markdown-mode-map (kbd "C-c C-v") 'wmmc/marked-markdown-preview)
       ))

  (setq ruby-deep-indent-paren nil)
  (setq ruby-use-smie nil)
  (setq ruby-insert-encoding-magic-comment nil)

  (setq deft-directory "~/Dropbox/notes")
  (setq org-startup-folded nil)

  )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (writeroom-mode robe orgit org-download magit-svn forge evil-nerd-commenter evil-magit dumb-jump doom-modeline docker cider sesman clojure-mode browse-at-remote ace-window ace-link counsel swiper reformatter helm ivy magit transient lv flycheck pythonic all-the-icons org-plus-contrib hydra yasnippet-snippets yapfify yaml-mode xterm-color ws-butler winum which-key wgrep web-mode web-beautify volatile-highlights visual-fill-column vi-tilde-fringe uuidgen use-package unfill toml-mode toc-org tagedit tablist systemd symon string-inflection sql-indent spaceline-all-the-icons smex smeargle slim-mode shrink-path shell-pop seeing-is-believing scss-mode sass-mode rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocop rspec-mode restart-emacs request rbenv ranger rainbow-delimiters racer queue pyvenv pytest pyenv-mode py-isort pug-mode projectile-rails prettier-js pippel pipenv pip-requirements persp-mode password-generator paradox ox-hugo ox-gfm overseer org-projectile org-present org-pomodoro org-mime org-journal org-bullets org-brain open-junk-file ob-restclient ob-http ob-elixir nix-mode neotree nameless mwim mvn multi-term move-text mmm-mode minitest meghanada maven-test-mode markdown-toc magithub magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode json-navigator json-mode js2-refactor js-doc ivy-yasnippet ivy-xref ivy-purpose ivy-hydra indent-guide importmagic impatient-mode hungry-delete hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation helm-make helm-core haskell-snippets groovy-mode groovy-imports graphviz-dot-mode gradle-mode google-translate golden-ratio godoctor go-tag go-rename go-impl go-guru go-gen-test go-fill-struct go-eldoc gnuplot gitignore-templates gitignore-mode github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md fuzzy font-lock+ flyspell-correct-ivy flycheck-rust flycheck-pos-tip flycheck-mix flycheck-haskell flycheck-elm flycheck-credo flx-ido fill-column-indicator feature-mode fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-org evil-numbers evil-mc evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eshell-z eshell-prompt-extras esh-help erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks ensime emmet-mode elm-test-runner elm-mode elisp-slime-nav elfeed-web elfeed-org elfeed-goodies eldoc-eval editorconfig dotenv-mode dockerfile-mode docker-tramp diminish diff-hl deft define-word dash-at-point cython-mode csv-mode counsel-projectile counsel-dash counsel-css company-web company-terraform company-tern company-statistics company-restclient company-quickhelp company-nixos-options company-go company-ghci company-emacs-eclim company-cabal company-anaconda column-enforce-mode cmm-mode closql clojure-snippets clean-aindent-mode cider-eval-sexp-fu chruby centered-cursor-mode cargo bundler auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile alchemist aggressive-indent ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
