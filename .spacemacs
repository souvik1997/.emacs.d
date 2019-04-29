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
   '(
     octave
     csv
     rust
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ivy
     helm
     auto-completion
     better-defaults
     emacs-lisp
     git
     ;; markdown
     themes-megapack
     org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     syntax-checking
     ;; languages:
     (latex :variables latex-enable-auto-fill t latex-build-command "LatexMk")
     c-c++
     elixir
     erlang
     javascript
     python
     markdown
     html
     haskell
     emacs-lisp
     java
     asm
     ;; version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(ag)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
                                    vi-tilde-fringe
                                    fill-column-indicator
                                    )
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
   dotspacemacs-editing-style 'emacs
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
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
   dotspacemacs-themes '(
                         majapahit-dark)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
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
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
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
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
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
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
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
   dotspacemacs-loading-progress-bar nil
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
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
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
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
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'all
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (setq-default dotspacemacs-themes '(spacemacs-dark))
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (spaceline-toggle-purpose-off)
  (setq powerline-default-separator (quote bar))
  (setq powerline-height 30)
  (spaceline-compile)

  ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)
  (require 'recentf)
  (global-set-key (kbd "C-x C-r") 'counsel-recentf)
  (global-set-key (kbd "C-x C-b") 'switch-to-buffer)

  (require 'cc-mode)
  (c-set-offset 'case-label '+)
  (c-set-offset 'access-label -1)
  (setq auto-mode-alist(cons '("\\.h$"   . c++-mode)  auto-mode-alist))

  (setq latex-enable-auto-fill nil)
  (remove-hook 'LaTeX-mode-hook 'latex/auto-fill-mode)

  (global-hl-line-mode -1)


  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
  (require 'magit)
  (global-set-key (kbd "C-x g") 'magit-status)

  (require 'helm)
  (global-set-key (kbd "C-x C-i") 'helm-imenu)

  (require 'mouse)
  (xterm-mouse-mode 1)
  (global-set-key [mouse-4] '(lambda ()
                               (interactive)
                               (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                               (interactive)
                               (scroll-up 1)))
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t)

  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))

  (setq mouse-wheel-scroll-amount '(3 ((shift) . 0.6)))
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

  (spacemacs/toggle-which-key-off)
  (setq company-idle-delay 1.0)

  (setq LaTeX-verbatim-environments-local '("Verbatim" "lstlisting"))

  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-s") 'isearch-forward)

  (global-set-key [C-return] [return])
  (global-unset-key [(control z)])
  (global-unset-key [(control x)(control z)])
  (global-set-key [(control z)] 'undo)
  (with-eval-after-load 'org (org-defkey org-mode-map [(meta return)] 'org-meta-return))
  (defadvice yes-or-no-p (around prevent-dialog activate)
    "Prevent yes-or-no-p from activating a dialog"
    (let ((use-dialog-box nil))
      ad-do-it))
  (defadvice y-or-n-p (around prevent-dialog-yorn activate)
    "Prevent y-or-n-p from activating a dialog"
    (let ((use-dialog-box nil))
      ad-do-it))

  (require 'expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region)

  (setq exec-path (append exec-path '("/Library/TeX/texbin" "/opt/local/bin")))

  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-item-indent 0)
 '(LaTeX-verbatim-environments (quote ("verbatim" "verbatim*" "lstlisting")))
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-term-color-vector
   [unspecified "#081724" "#ff694d" "#68f6cb" "#fffe4e" "#bad6e2" "#afc0fd" "#d2f1ff" "#d3f9ee"] t)
 '(company-idle-delay 0.8)
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("d6922c974e8a78378eacb01414183ce32bc8dbf2de78aabcc6ad8172547cb074" "0c387e27a3dd040b33c6711ff92e13bd952369a788eee97e4e4ea2335ac5528f" "3632cf223c62cb7da121be0ed641a2243f7ec0130178722554e613c9ab3131de" "17cda1304ba8d26d62bf247cab2c161d12957054b6be4477abb5972a74eea4e1" "191a0b83e410ff9f7be7dbb158a0bb8fce32211167919a6ab5f414a906df4d7d" "3a5f04a517096b08b08ef39db6d12bd55c04ed3d43b344cf8bd855bde6d3a1ae" "ba6f8606cae826654d4c2242363f2042dd35c1595ef6367ad44d5f42851e6aa9" "32e3693cd7610599c59997fee36a68e7dd34f21db312a13ff8c7e738675b6dfc" "a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "b825687675ea2644d1c017f246077cdd725d4326a1c11d84871308573d019f67" "62c80c6889010c3f0656e81ae201754058fd44743076f8dc56c595c2b9b5e298" "cedd3b4295ac0a41ef48376e16b4745c25fa8e7b4f706173083f16d5792bb379" "a56a6bf2ecb2ce4fa79ba636d0a5cf81ad9320a988ec4e55441a16d66b0c10e0" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "7e376fb329a0e46a04e8285b0e45199a083f98c69b0e1039ec1cb1d366e66e9c" default)))
 '(evil-escape-key-sequence "0000000````")
 '(evil-toggle-key "C-`")
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#20240E" t)
 '(flycheck-gcc-language-standard "c++1y")
 '(fringe-mode 6 nil (fringe))
 '(global-spacemacs-leader-override-mode t)
 '(global-spacemacs-whitespace-cleanup-mode t)
 '(helm-autoresize-max-height 20)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-symbol-colors
   (quote
    ("#EFFF00" "#73CD4F" "#83DDFF" "MediumPurple1" "#66CDAA" "DarkOrange" "HotPink1" "#809FFF" "#ADFF2F")))
 '(highlight-tail-colors
   (quote
    (("#20240E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#20240E" . 100))))
 '(hl-paren-background-colors
   (quote
    ("#00FF99" "#CCFF99" "#FFCC99" "#FF9999" "#FF99CC" "#CC99FF" "#9999FF" "#99CCFF" "#99FFCC" "#7FFF00")))
 '(hl-paren-colors (quote ("#326B6B")))
 '(inhibit-startup-screen nil)
 '(ivy-height 8)
 '(linum-format " %7d |")
 '(magit-diff-use-overlays nil)
 '(org-src-block-faces (quote (("emacs-lisp" (:background "#F0FFF0")))))
 '(package-selected-packages
   (quote
    (white-sand-theme rebecca-theme org-mime exotica-theme winum unfill solarized-theme madhat2r-theme fuzzy flycheck-credo nix-mode toml-mode racer flycheck-rust seq cargo rust-mode kaolin-theme csv-mode auctex-latexmk powerline spinner hydra parent-mode helm helm-core flx smartparens iedit anzu evil goto-chg undo-tree highlight diminish autothemer projectile bind-map bind-key packed async f pkg-info epl s dash avy popup package-build ag flycheck-google-cpplint counsel swiper ivy yapfify x86-lookup web-mode web-beautify tagedit slim-mode scss-mode sass-mode pyvenv pytest pyenv-mode py-isort pug-mode pip-requirements ob-elixir nasm-mode mmm-mode markdown-toc markdown-mode livid-mode skewer-mode simple-httpd live-py-mode less-css-mode json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc intero hy-mode hlint-refactor hindent helm-pydoc helm-hoogle helm-css-scss haskell-snippets haml-mode gh-md flycheck-mix flycheck-haskell erlang emmet-mode disaster cython-mode company-web web-completion-data company-tern dash-functional tern company-ghci company-ghc ghc haskell-mode company-emacs-eclim eclim company-cabal company-c-headers company-auctex company-anaconda coffee-mode cmm-mode cmake-mode clang-format auctex anaconda-mode pythonic alchemist elixir-mode zonokai-theme zenburn-theme zen-and-art-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pastels-on-dark-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme firebelly-theme farmhouse-theme espresso-theme dracula-theme django-theme darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme smeargle orgit org-projectile pcache org-present org org-pomodoro alert log4e gntp org-download mwim magit-gitflow htmlize helm-themes helm-swoop helm-projectile helm-mode-manager helm-gitignore helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link flycheck-pos-tip pos-tip flycheck evil-magit magit magit-popup git-commit with-editor company-statistics company auto-yasnippet yasnippet ace-jump-helm-line ac-ispell auto-complete ws-butler window-numbering which-key wgrep volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spacemacs-theme spaceline smex restart-emacs request rainbow-delimiters quelpa popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint ivy-hydra info+ indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-make google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump define-word counsel-projectile column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(powerline-color1 "#3d3d68")
 '(powerline-color2 "#292945")
 '(powerline-default-separator (quote bar))
 '(powerline-height 20)
 '(projectile-keymap-prefix "p")
 '(safe-local-variable-values
   (quote
    ((TeX-command-extra-options . "-shell-escape")
     (TeX-engine . default-shell-escape)
     (add-to-list
      (quote auto-mode-alist)
      (quote
       ("\\.h\\'" . c++-mode)))
     (elixir-enable-compilation-checking . t)
     (elixir-enable-compilation-checking))))
 '(scroll-margin 10)
 '(shell-file-name "/bin/bash")
 '(spaceline-helm-mode t)
  '(spaceline-info-mode t)
  '(tramp-default-method "ssh")
  '(vc-annotate-background nil)
  '(vc-annotate-color-map
    (quote
     ((20 . "#F92672")
      (40 . "#CF4F1F")
      (60 . "#C26C0F")
      (80 . "#E6DB74")
      (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(vc-follow-symlinks nil)
 '(weechat-color-list
   (unspecified "#272822" "#20240E" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(which-key-idle-delay 2.0)
 '(xterm-color-names
   ["#303030" "#D66F84" "#D79887" "#D49A8A" "#94B1A3" "#A8938C" "#989584" "#BAB2A9"])
 '(xterm-color-names-bright
   ["#3A3A3A" "#E47386" "#CC816B" "#769188" "#7D6F6A" "#9C8772" "#BAB2A9"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-cursor ((t (:background "gray60" :foreground "white"))))
 '(powerline-active1 ((t (:background "gray39" :foreground "#b2b2b2"))))
 '(powerline-active2 ((t (:background "gray44" :foreground "#b2b2b2"))))
 '(spacemacs-emacs-face ((t (:background "#31423A" :foreground "#222226" :inherit (quote mode-line))))))
