;; -*- mode: emacs-lisp -*-
;; https://github.com/sprig/org-capture-extension
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
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'nil
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(sql
     haskell
     gtags
     python
     c-c++
     (elfeed :variables
             elfeed-feeds '(
                            "https://herald.community.rs/rss"
                            "https://feeds.feedburner.com/37signals/beMH"
                            "https://www.joelonsoftware.com/feed/"
                            "http://feeds.haacked.com/haacked/"
                            "https://feeds.feedburner.com/codinghorror"
                            "http://thunk.org/tytso/blog/feed/"
                            "http://www.aaronsw.com/2002/feeds/pgessays.rss"
                            "https://www.reddit.com/r/programming/.rss"
                            "https://pragprog.com/feed/global"
                            "http://secretgeek.net/Rss"
                            "https://www.joelonsoftware.com/feed/"
                            "https://feeds.feedburner.com/ajaxian"
                            "https://feeds.feedburner.com/SteveysBlogRants"
                            "http://feeds.reuters.com/reuters/UKTopNews"

                            ))
     (colors :variables colors-colorize-identifiers nil
             colors-enable-nyan-cat-progress-bar t
             )
     xkcd
     selectric
     ibuffer
     fasd
     (erc :variables
          erc-server-list
          '(("irc.freenode.net"
             :port "6697"
             :ssl t
             :nick "a-person"
             :password "secret")))
     (mu4e :variables
           mu4e-installation-path "/usr/share/emacs/site-lisp")
     javascript
     html
     rust
     yaml
     emoji
     helm
     (better-defaults :variables
                      better-defaults-move-to-beginning-of-code-first t)
     emacs-lisp
     markdown
     (org :variables org-enable-github-support t)
     (auto-completion :variables
                      auto-completion-return-key-behavior nil;;'complete
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-private-snippets-directory (concat dotspacemacs-directory "snippets")
                      ;; setq auto-completion-enable-snippets-in-popup nil ;;t
                      ;; aya-persist-snippets-dir (concat dotspacemacs-directory "snippets")
                      )
     (shell :variables
            shell-default-shell 'eshell
            shell-enable-smart-eshell t
            shell-default-height 30
            shell-default-position 'bottom)
     version-control
     git
     github
     ;; spell-checking
     syntax-checking
     ;; languages
     (ruby :variables
           ruby-test-runner 'rspec)
     elixir
     ;; frameworks
     ruby-on-rails
     ;; random
     dash
     restclient
     deft
     games
     search-engine
     (wakatime :variables wakatime-cli-path "/usr/local/bin/wakatime")
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      ;; pretty-mode
                                      ruby-hash-syntax
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'random
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.

   ;; (set-face-attribute 'default nil :family "Source Code Pro")
   ;; (set-face-attribute 'default nil :family "Haskelig")
   ;; (set-face-attribute 'default nil :height 165)
   ;; (set-face-attribute 'default nil :family "Fira Code Retina")
   ;; (mac-auto-operator-composition-mode)
   dotspacemacs-default-font '("Fira Code" ;;"Source Code Pro"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.0)

   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
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
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non-nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
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
   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non-nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
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
   dotspacemacs-maximized-at-startup t
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
   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
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
   dotspacemacs-highlight-delimiters 'all
   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
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
   dotspacemacs-frame-title-format "%I@%S"
   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed
   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil
   dotspacemacs-delete-orphan-packages nil
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
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."


  ;; qqq

  (setq magithub-debug t)

  ;; Set to the location of your Org files on your local system
  (setq org-directory "~/org")
  ;; Set to the name of the file where new notes will be stored
  (setq org-mobile-inbox-for-pull "~/org/todo.org")
  ;; Set to <your Dropbox root directory>/MobileOrg.
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")




  (eval-after-load 'rspec-mode
    '(define-key rspec-compilation-mode-map (kbd "C-x C-q")
       'inf-ruby-switch-from-compilation))

  ;; (add-hook 'rspec-compilation-mode
  ;;           (lambda ()
  ;;             (inf-ruby-switch-from-compilation)))

  ;; (add-hook 'inf-ruby-mode
  ;;           (lambda ()
  ;;                 (inf-ruby-switch-from-compilation)))

  (defun wmmc/find-next-file (&optional backward)
    "Find the next file (by name) in the current directory.

With prefix arg, find the previous file."
    (interactive "P")
    (when buffer-file-name
      (let* ((file (expand-file-name buffer-file-name))
             (files (cl-remove-if (lambda (file) (cl-first (file-attributes file)))
                                  (sort (directory-files (file-name-directory file) t nil t) 'string<)))
             (pos (mod (+ (cl-position file files :test 'equal) (if backward -1 1))
                       (length files))))
        (find-file (nth pos files)))))

  ;; (defun wmmc/find-previous-file
  ;;     (interactive)
  ;;   (funcall-interactively
  ;;    wmmc/find-next-file))

  (evil-leader/set-key "on" 'wmmc/find-next-file)
  ;; (evil-leader/set-key "op" 'wmmc/find-previous-file)

  ;; NB template selector BEGIN_SRC block == <s + TAB

  ;; (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (setq erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (setq erc-lurker-threshold-time 3600)


  (require 'ruby-hash-syntax)


  ;; fix annoying rspec-compliation mode thing where you can't go left
  (evil-declare-key 'normal rspec-compilation-mode-map (kbd "h") 'evil-backward-char)
  (evil-declare-key 'normal cargo-process-mode-map (kbd "h") 'evil-backward-char)
  (evil-declare-key 'normal rspec-compilation-mode-map (kbd "l") 'evil-forward-char)
  (evil-declare-key 'normal cargo-process-mode-map (kbd "l") 'evil-forward-char)

  (evil-declare-key 'visual rspec-compilation-mode-map (kbd "h") 'evil-backward-char)
  (evil-declare-key 'visual cargo-process-mode-map (kbd "h") 'evil-backward-char)
  (evil-declare-key 'visual rspec-compilation-mode-map (kbd "l") 'evil-forward-char)
  (evil-declare-key 'visual cargo-process-mode-map (kbd "l") 'evil-forward-char)

  ;; (define-key evil-insert-state-map "\C-e" 'end-of-line)

  ;; Set fish as the default shell
  ;; (setq explicit-shell-file-name (replace-regexp-in-string "\n$" ""
  ;;                                                          (shell-command-to-string "which fish || which bash")))

  ;; = + = => ==
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                 (36 . ".\\(?:>\\)")
                 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 ;; (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)") ;; This causes helm to go mental... :|
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (48 . ".\\(?:x[a-zA-Z]\\)")
                 (58 . ".\\(?:::\\|[:=]\\)")
                 (59 . ".\\(?:;;\\|;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                 (91 . ".\\(?:]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (119 . ".\\(?:ww\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
                 )
               ))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring]))))

  (defun wmmc/secret-from-authinfo-host(host)
    (setq authinfo-row (
                        car (auth-source-search :host host
                                                :max 1
                                                :requires '(user secret))))
    (if authinfo-row (funcall (plist-get authinfo-row :secret)) nil))

  (setq wakatime-api-key (wmmc/secret-from-authinfo-host "wakatime.com"))

  (add-hook 'yas-minor-mode-hook
            (lambda ()
              (yas-activate-extra-mode 'fundamental-mode)))
  (yas-global-mode 1)

  ;; reload the tags file without always prompting
  (setq tags-revert-without-query 1)


  (setq aya-persist-snippets-dir "~/.spacemacs.d/snippets")

  (setq eshell-aliases-file "~/.spacemacs.d/eshell/alias")

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

  (spacemacs/toggle-indent-guide-globally-on)

  ;; \C-h is now DEL
  (define-key key-translation-map [?\C-h] [?\C-?])

  ;; follow symlinks
  (setq vc-follow-symlinks t)

  ;; swap mac keys
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)

  (global-set-key (kbd "C-M-H") 'backward-kill-word)

  ;; enable #
  (define-key key-translation-map (kbd "§") (kbd "#"))
  (define-key key-translation-map (kbd "s-3") (kbd "#"))
  ;; (define-key key-translation-map (kbd "M-3") (kbd "#"))
  ;; (define-key key-translation-map (kbd "M-£") (kbd "#"))
  ;; (define-key key-translation-map (kbd "H-3") (kbd "#"))
  ;; (define-key key-translation-map (kbd "H-£") (kbd "#"))
  ;; (define-key key-translation-map (kbd "S-3") (kbd "#"))
  ;; (define-key key-translation-map (kbd "S-£") (kbd "#"))

  ;; (global-set-key (kbd "s-3") `(lambda () (interactive) (insert "#")))

  (evil-declare-key 'normal dired-mode-map (kbd ";") 'dired-up-directory)

  (define-key evil-insert-state-map "\C-e" 'end-of-line)
  (define-key evil-visual-state-map "\C-e" 'end-of-line)
  (define-key evil-normal-state-map "\C-e" 'end-of-line)

  (evil-leader/set-key "ol"  'link-hint-open-link-at-point)

  (setq ruby-insert-encoding-magic-comment nil)

  ;; (setq shell-command-switch "-ic")

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


  ;;-------------------------------------------------------
  ;; begin sourcing of .bash_profile

  ;; only do this on Mac OS X
  (when (string= system-type "darwin")
    ;; require common lisp extensions, for search
    (require 'cl)


    (defun wmmc/src-shell-unescape (string)
      ;; replace \n \t \r \b \a \v \\
      ;; and octal escapes of the form \0nn

      (replace-regexp-in-string
       "\\\\\\([ntrbav]\\|\\(\\\\\\)\\|\\(0[0-7][0-7]\\)\\)"
       (lambda (str)
         ;; interpret octal expressions
         ;; of the form "\0nn"
         (let ((char1 (aref str 1)))
           (cond ((= ?0 (aref str 1))
                  (byte-to-string
                   (+ (* (- (aref str 2) ?0) 8)
                      (- (aref str 3) ?0))))
                 ((eq char1 ?n) "\n")
                 ((eq char1 ?t) "\t")
                 ((eq char1 ?r) "\r")
                 ((eq char1 ?b) "\b")
                 ((eq char1 ?a) "\a")
                 ((eq char1 ?v) "\v")
                 ((eq char1 ?\\) "\\\\")
                 (t "")))) string))

    (defun wmmc/src-set-environment-from-env-output(env-output)
      ;; set the environment from shell's "env" output
      (let ((lines (split-string env-output "\n" t)))
        (dolist (line lines)
          (let ((idx-equals (search "=" line)))
            (when (and (not (eq idx-equals nil))
                       (> idx-equals 1))
              (let  ((key (substring line 0 idx-equals))
                     (value (substring line (+ idx-equals 1))))
                (setenv key (wmmc/src-shell-unescape value))
                ;; (message "%s = %s" key value)
                ))))))

    (defun wmmc/src-source-shell-file (file-name)
      ;; if your shell is sh rather than bash, the "source " may need
      ;; to be ". " instead
      (let* ((command (concat "source '"  file-name "'; echo 'post-env'; env"))
             (output (shell-command-to-string command))
             (idx-post-env (search "post-env" output)))
        (if (eq nil idx-post-env)
            (message "Didn't find expected output after sourcing %s. Found: %s" file-name output)
          (let ((trimmed-output (substring output idx-post-env)))
            ;; (message "trimmed-output: %s" trimmed-output)
            (wmmc/src-set-environment-from-env-output trimmed-output)))))


    (wmmc/src-source-shell-file (expand-file-name "~/.bash_profile")))
  ;; end sourcing of .bash_profile
  (define-key evil-normal-state-map (kbd "p") 'evil-paste-after)
  (define-key evil-normal-state-map (kbd "P") 'evil-paste-before)

  (with-eval-after-load 'sql-mode
    (setq sql-postgres-login-params (append sql-postgres-login-params '(port))))

  (with-eval-after-load 'org

    (require 'org-capture)
    (require 'org-protocol)

    (server-start)

    ;; dirty hack
    (require 'org-capture)
    (setq org-modules (quote (org-protocol)))
    (require 'org-protocol)

    ;; (setq org-capture-templates '(
    ;;                               ("p" "Protocol" entry (file+headline "~/org/notes.org" "Inbox")
    ;;                                Title "* \nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
    ;;                               ("L" "Protocol Link" entry (file+headline "~/org/notes.org" "Inbox")
    ;;                                "* %? [[%:link][%:description]] \nCaptured On: %U")
    ;;                               ))


    (setq org-capture-templates `(
                                  ("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
                                   "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
                                  ("L" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
                                   "* %? [[%:link][%:description]] \nCaptured On: %U")
                                  ))

    ;; (add-to-list 'org-modules 'org-protocol)

    ;; (with-eval-after-load 'org-agenda
    ;;   (require 'org-projectile)
    ;;   )

    ;; (setq org-agenda-files '("~/org"))
    (setq org-agenda-files (list "~/org/todo.org"))

    ;;(push (org-projectile:todo-files) org-agenda-files))
    ;; (setq org-agenda-files (append org-agenda-files (org-projectile:todo-files))))

    (org-babel-do-load-languages
     'org-babel-load-languages '((C . t)
                                 (plantuml . t)
                                 (ruby . t)
                                 (shell . t)
                                 (http . t)
                                 (js . t)
                                 (sh . t)
                                 ))
    ;; (setq org-agenda-files (concat org-directory "/todo.org"))
    ;; (setq org-default-notes-file (concat org-directory "/notes.org"))

    ;;    find ~/.emacs.d/elpa/org* -name "*elc" -delete

    ;; (setq org-capture-templates '(
    ;;                               ("p" "Protocol" entry (file+headline "~/org/notes.org" "Inbox")
    ;;                                "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
    ;;                               ("L" "Protocol Link" entry (file+headline "~/org/notes.org" "Inbox")
    ;;                                "* %? [[%:link][%:description]] \nCaptured On: %U")))

    ;; (setq org-capture-templates
    ;;       '(("t" "Todo" entry (file+headline (concat org-directory "/todo.org")) ;; "Tasks")
    ;;          "* TODO %?\n  %i\n  %a")
    ;;         ("j" "Journal" entry (file+datetree  (concat org-directory "/journal.org") )
    ;;          "* %?\nEntered on %U\n  %i\n  %a")))

    )
  (show-paren-mode t)


  ;; ;; =mu4e


  (setq mu4e-maildir "~/.mail"
        mu4e-trash-folder "/trash"
        mu4e-refile-folder "/archive"
        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval nil
        mu4e-compose-signature-auto-include nil
        mu4e-compose-dont-reply-to-self t
        mu4e-view-show-images t
        mu4e-view-show-addresses t

        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "/usr/local/bin/msmtp"
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-sendmail-f-is-evil 't

        mu4e-user-mail-address-list "wmmclarke@gmail.com"
        ;; user-mail-address "wmmclarke@gmail.com"
        ;; user-full-name "William Clarke"
        )

;;; Mail directory shortcuts
  (setq mu4e-maildir-shortcuts
        '(("/gmail/INBOX" . ?g)
          ("/gmail/Later" . ?l)
          ("date:today..now AND maildir:/gmail/Archive" . ?g)
          ("flag:unread AND NOT flag:trashed" . ?u)
          ("NOT maildir:/archive AND NOT maildir:'/[Gmail].All Mail' AND date:today" . ?b)))
  ;; ("date:today..now AND maildir:/gmail/Archive" "Gmail Today" ?g)
  ;; ("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
  ;; ("NOT maildir:/archive AND NOT maildir:'/[Gmail].All Mail' AND date:today" "Unread Today" ?b)))

;;; Bookmarks
  (setq mu4e-bookmarks
        `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
          ("date:today..now" "Today's messages" ?t)
          ("date:7d..now" "Last 7 days" ?w)
          ("mime:image/*" "Messages with images" ?p)
          (,(mapconcat 'identity
                       (mapcar
                        (lambda (maildir)
                          (concat "maildir:" (car maildir)))
                        mu4e-maildir-shortcuts) " OR ")
           "All inboxes" ?i)))


  (defun wmmc/current-chrome-url ()
    (interactive)
    (do-applescript
     (concat
      "set frontmostApplication to path to frontmost application\n"
      "tell application \"Google Chrome\"\n"
      " set theUrl to get URL of active tab of first window\n"
      " set theResult to (get theUrl)\n"
      "end tell\n"
      "return theResult as string\n"
      )
     )
    )

  ;; (shell-command-to-string "source ~/.bash_profile")
  ;; (setenv "PATH" (shell-command-to-string "source ~/.bash_profile")) ;;; echo -n $PATH"))

  ;; Do not write anything past this comment. This is where Emacs will
  ;; auto-generate custom variable definitions.
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
     '(evil-want-Y-yank-to-eol t)
     '(gud-gdb-command-name "gdb --annotate=1")
     '(large-file-warning-threshold nil)
     '(package-selected-packages
       (quote
        (helm-hoogle dante ghc haskell-mode pretty-mode password-store wakatime-mode typit mmt sudoku pacmacs engine-mode 2048-game bm hackernews org-brain impatient-mode evil-org counsel swiper ivy yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic gmail-message-mode ham-mode html-to-markdown flymd edit-server fasd elfeed-web elfeed-org elfeed-goodies ace-jump-mode noflet elfeed xkcd selectric-mode rainbow-mode rainbow-identifiers color-identifiers-mode typo ibuffer-projectile powerline pcre2el spinner gntp parent-mode window-purpose imenu-list helm-gitignore request helm-c-yasnippet fringe-helper git-gutter+ git-gutter seq pos-tip flx iedit anzu goto-chg undo-tree highlight bind-map bind-key pkg-info epl auto-complete popup ruby-refactor add-node-modules-path ox-gfm deft dash evil-lion password-generator ghub+ apiwrap ghub helm-company emojify editorconfig packed git-commit markdown-mode alert async s diminish smartparens evil flycheck company yasnippet avy magit magit-popup with-editor log4e org-plus-contrib projectile hydra f helm helm-core symon string-inflection browse-at-remote skewer-mode simple-httpd json-snatcher json-reformat multiple-cursors js2-mode dash-functional tern helm-css-scss haml-mode web-completion-data rust-mode ob-elixir flycheck-mix flycheck-credo alchemist elixir-mode gh marshal logito pcache ht restclient-helm inflections helm-dash restclient know-your-http-well rake inf-ruby helm-themes helm-swoop helm-purpose helm-projectile helm-mode-manager helm-flx helm-descbinds helm-ag evil-unimpaired ace-jump-helm-line yaml-mode xterm-color ws-butler winum which-key wgrep web-mode web-beautify volatile-highlights vimrc-mode vi-tilde-fringe uuidgen use-package unfill toml-mode toc-org tagedit swift-mode sql-indent spaceline smex smeargle slim-mode slack shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs rbenv rainbow-delimiters racer pug-mode projectile-rails popwin persp-mode paradox orgit org-projectile org-present org-pomodoro org-download org-bullets open-junk-file ob-restclient ob-http neotree mwim multi-term mu4e-maildirs-extension mu4e-alert move-text mmm-mode minitest markdown-toc magithub magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc ivy-purpose ivy-hydra intero insert-shebang info+ indent-guide hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-make haskell-snippets google-translate golden-ratio gnuplot gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md fuzzy flycheck-rust flycheck-pos-tip flycheck-haskell flx-ido fish-mode fill-column-indicator feature-mode fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help erlang erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks emoji-cheat-sheet-plus emmet-mode elisp-slime-nav dumb-jump disaster diff-hl define-word dash-at-point dactyl-mode counsel-projectile counsel-dash company-web company-tern company-statistics company-shell company-restclient company-ghci company-ghc company-emoji company-cabal company-c-headers column-enforce-mode coffee-mode cmm-mode cmake-mode clean-aindent-mode clang-format chruby cargo bundler auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ac-ispell))))
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     )
    ))
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
   '(evil-want-Y-yank-to-eol t)
   '(gud-gdb-command-name "gdb --annotate=1")
   '(large-file-warning-threshold nil)
   '(org-agenda-files
     (quote
      ("/Users/williamclarke/org/todo.org" "/Users/williamclarke/org/backup.org" "/Users/williamclarke/org/stuff.org")))
   '(package-selected-packages
     (quote
      (helm-hoogle dante ghc haskell-mode pretty-mode password-store wakatime-mode typit mmt sudoku pacmacs engine-mode 2048-game bm hackernews org-brain impatient-mode evil-org counsel swiper ivy yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic gmail-message-mode ham-mode html-to-markdown flymd edit-server fasd elfeed-web elfeed-org elfeed-goodies ace-jump-mode noflet elfeed xkcd selectric-mode rainbow-mode rainbow-identifiers color-identifiers-mode typo ibuffer-projectile powerline pcre2el spinner gntp parent-mode window-purpose imenu-list helm-gitignore request helm-c-yasnippet fringe-helper git-gutter+ git-gutter seq pos-tip flx iedit anzu goto-chg undo-tree highlight bind-map bind-key pkg-info epl auto-complete popup ruby-refactor add-node-modules-path ox-gfm deft dash evil-lion password-generator ghub+ apiwrap ghub helm-company emojify editorconfig packed git-commit markdown-mode alert async s diminish smartparens evil flycheck company yasnippet avy magit magit-popup with-editor log4e org-plus-contrib projectile hydra f helm helm-core symon string-inflection browse-at-remote skewer-mode simple-httpd json-snatcher json-reformat multiple-cursors js2-mode dash-functional tern helm-css-scss haml-mode web-completion-data rust-mode ob-elixir flycheck-mix flycheck-credo alchemist elixir-mode gh marshal logito pcache ht restclient-helm inflections helm-dash restclient know-your-http-well rake inf-ruby helm-themes helm-swoop helm-purpose helm-projectile helm-mode-manager helm-flx helm-descbinds helm-ag evil-unimpaired ace-jump-helm-line yaml-mode xterm-color ws-butler winum which-key wgrep web-mode web-beautify volatile-highlights vimrc-mode vi-tilde-fringe uuidgen use-package unfill toml-mode toc-org tagedit swift-mode sql-indent spaceline smex smeargle slim-mode slack shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs rbenv rainbow-delimiters racer pug-mode projectile-rails popwin persp-mode paradox orgit org-projectile org-present org-pomodoro org-download org-bullets open-junk-file ob-restclient ob-http neotree mwim multi-term mu4e-maildirs-extension mu4e-alert move-text mmm-mode minitest markdown-toc magithub magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc ivy-purpose ivy-hydra intero insert-shebang info+ indent-guide hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-make haskell-snippets google-translate golden-ratio gnuplot gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md fuzzy flycheck-rust flycheck-pos-tip flycheck-haskell flx-ido fish-mode fill-column-indicator feature-mode fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help erlang erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks emoji-cheat-sheet-plus emmet-mode elisp-slime-nav dumb-jump disaster diff-hl define-word dash-at-point dactyl-mode counsel-projectile counsel-dash company-web company-tern company-statistics company-shell company-restclient company-ghci company-ghc company-emoji company-cabal company-c-headers column-enforce-mode coffee-mode cmm-mode cmake-mode clean-aindent-mode clang-format chruby cargo bundler auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ac-ispell)))
   '(wakatime-python-bin nil))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   )
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil)
 '(package-selected-packages
   (quote
    (mmt rake inflections pcre2el spinner org-category-capture alert log4e gntp markdown-mode skewer-mode json-snatcher json-reformat multiple-cursors js2-mode intero hydra htmlize hlint-refactor hindent parent-mode projectile helm-hoogle helm-gtags request haskell-snippets haml-mode gitignore-mode fringe-helper git-gutter+ git-gutter gh marshal logito pcache ht ggtags seq pos-tip flycheck-haskell flycheck flx magit magit-popup git-commit with-editor smartparens iedit anzu evil goto-chg undo-tree highlight simple-httpd org-plus-contrib ace-jump-mode noflet powerline popwin elfeed diminish web-completion-data dash-functional tern restclient know-your-http-well company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode rust-mode inf-ruby bind-map bind-key yasnippet packed anaconda-mode pythonic f s company dash elixir-mode pkg-info epl helm avy helm-core async auto-complete popup github-browse-file yapfify yaml-mode xterm-color xkcd ws-butler winum which-key web-mode web-beautify wakatime-mode volatile-highlights vi-tilde-fringe uuidgen use-package unfill typit toml-mode toc-org tagedit symon sudoku string-inflection sql-indent spaceline smeargle slim-mode shell-pop selectric-mode scss-mode sass-mode rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocop rspec-mode robe restclient-helm restart-emacs realgud rbenv rainbow-mode rainbow-identifiers rainbow-delimiters racer pyvenv pytest pyenv-mode py-isort pug-mode projectile-rails pip-requirements persp-mode password-store password-generator paradox pacmacs ox-gfm orgit org-projectile org-present org-pomodoro org-download org-bullets org-brain open-junk-file ob-restclient ob-http ob-elixir neotree mwim multi-term mu4e-maildirs-extension mu4e-alert move-text mmm-mode minitest markdown-toc magithub magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc info+ indent-guide impatient-mode ibuffer-projectile hy-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-purpose helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gmail-message-mode github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md fuzzy flymd flycheck-rust flycheck-pos-tip flycheck-mix flycheck-credo flx-ido fill-column-indicator feature-mode fasd fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks engine-mode emojify emoji-cheat-sheet-plus emmet-mode elisp-slime-nav elfeed-web elfeed-org elfeed-goodies editorconfig edit-server dumb-jump disaster diff-hl deft define-word dash-at-point cython-mode company-web company-tern company-statistics company-restclient company-emoji company-c-headers company-anaconda column-enforce-mode color-identifiers-mode coffee-mode cmake-mode clean-aindent-mode clang-format chruby cargo bundler browse-at-remote bm auto-yasnippet auto-highlight-symbol auto-compile alchemist aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell 2048-game))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
