;; Package-specific configuration
;; Package archives
(prefer-coding-system 'utf-8)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(asm-comment-char 35)
 '(battery-mode-line-format "%p%% %b ")
 '(c-basic-offset 2)
 '(c-default-style
   (quote
    ((c-mode . "ellemtel")
     (java-mode . "ellemtel")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(company-begin-commands
   (quote
    (self-insert-command org-self-insert-command orgtbl-self-insert-command)))
 '(company-global-modes (quote (not gud-mode org-mode erc-mode eshell-mode)))
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 1)
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes t)
 '(display-time-mode t)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(erc-nick "SouvikB")
 '(erc-nick-uniquifier "_")
 '(erc-server "")
 '(fci-rule-color "#383838")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js2-idle-timer-delay 1)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-export-preserve-breaks t)
 '(org-startup-indented t)
 '(org-support-shift-select (quote always))
 '(package-selected-packages
   (quote
    (wolfram-mode color-theme-sanityinc-tomorrow llvm-mode keyfreq erlang slime-company slime geiser py-autopep8 gitignore-mode nodejs-repl ghc x86-lookup info+ ranger haskell-mode ag isearch-prop flx-isearch isearch+ 2048-game browse-kill-ring+ easy-kill-extras easy-kill ein switch-window monokai-theme auctex-latexmk srefactor exec-path-from-shell elpy misc-cmds company auctex web-mode visible-mark spacemacs-theme spaceline smooth-scrolling smex projectile neotree markdown-mode magit js2-mode ido-yes-or-no ido-ubiquitous flycheck flx-ido dired+ company-web company-math company-auctex anzu)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(powerline-default-separator (quote bar))
 '(powerline-height 20)
 '(projectile-keymap-prefix "p")
 '(rcirc-default-full-name "SouvikB")
 '(rcirc-default-nick "SouvikB")
 '(rcirc-default-user-name "SouvikB")
 '(rcirc-server-alist nil)
 '(require-final-newline (quote visit-save))
 '(send-mail-function (quote sendmail-send-it))
 '(user-mail-address "souvik1997@gmail.com")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(verilog-align-ifelse t)
 '(verilog-auto-lineup (quote ignore))
 '(verilog-auto-newline nil)
 '(verilog-indent-level 4)
 '(verilog-indent-level-behavioral 4)
 '(verilog-indent-level-declaration 4)
 '(verilog-indent-level-directive 4)
 '(verilog-indent-level-module 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "#222226" :foreground "#b2b2b2" :box nil))))
 '(mode-line-inactive ((t (:background "#292b2e" :foreground "#b2b2b2" :box nil))))
 '(term-color-black ((t (:background "dark gray" :foreground "gray"))))
 '(term-color-blue ((t (:background "deep sky blue" :foreground "cyan")))))

(package-install-selected-packages)
(require 'server)
(unless (server-running-p) (server-start))

(setq ring-bell-function 'ignore)


;; Midnight-mode
(require 'midnight)

;; Mouse configuration
(setq mouse-wheel-scroll-amount '(3 ((shift) . 0.6)))
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

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

;; TRAMP mode
(setq tramp-default-method "ssh")

;; tls
(require 'tls)

;; Backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Hook up emacs clipboard with mac system clipboard
(defun copy-from-osx ()
  "Handle copy/paste intelligently on osx."
  (let ((pbpaste (purecopy "/usr/bin/pbpaste")))
    (if (and (eq system-type 'darwin)
             (file-exists-p pbpaste))
        (let ((tramp-mode nil)
              (default-directory "~"))
          (shell-command-to-string pbpaste)))))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(if (eq system-type 'darwin)
    (setq interprogram-cut-function 'paste-to-osx))
(if (eq system-type 'darwin)
    (setq interprogram-paste-function 'copy-from-osx))

;; PATH, dired, and exec-path configuration
;; Must be before package installation or certain commands like gzip won't be found
(if (eq system-type 'darwin) (setq insert-directory-program "/usr/local/bin/gls"))
(setq dired-listing-switches "-aBhl --group-directories-first")
(getenv "PATH")
 (setenv "PATH"
(concat
 "/Library/TeX/texbin" ":" "/usr/local/bin/" ":" "/usr/local/MacGPG2/bin" ":" "/usr/local/opt/coreutils/libexec/gnubin" ":" "/usr/bin/" ":" "~/.local/bin" ":"
 (getenv "PATH")))
(if (eq system-type 'darwin) (progn
                               (exec-path-from-shell-initialize)
                               (exec-path-from-shell-copy-env "PYTHONPATH")))


;; ranger
(setq ranger-override-dired t)

;; elpy
(elpy-enable)
(eval-after-load "elpy"
  '(cl-dolist (key '("M-<up>" "M-<down>" "M-<left>" "M-<right>"))
     (define-key elpy-mode-map (kbd key) nil)))

;; keyfreq
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;; AUCTeX
;(load "auctex.el" nil t t)
;(load "preview-latex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq preview-gs-command "/usr/local/bin/gs")
(setq LaTeX-item-indent 0)

;; AUCTeX-Latexmk
(require 'auctex-latexmk)
(auctex-latexmk-setup)
(setq auctex-latexmk-inherit-TeX-PDF-mode t)

;; Neotree
(require 'neotree)
(setq neo-smart-open t)
(global-set-key (kbd "C-c C-k") 'neotree-toggle)
(setq neo-vc-integration nil)



;; Smooth-scrolling
(require 'smooth-scrolling)

;; js2-mode
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac2-js2-mode)
(setq-default js2-global-externs '("module" "require" "jQuery" "$" "_" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
(setq-default js2-auto-indent-p t)
(setq-default js2-bounce-indent-p t)
(setq-default js2-basic-offset 2)
(setq js2-mode-hook
  '(lambda () (progn
    (set-variable 'indent-tabs-mode nil))))

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; spaceline
(require 'spaceline-config)
;; Fix broken separators on OS X
(setq ns-use-srgb-colorspace nil)
(setq spaceline-minor-modes-separator " ")
(spaceline-spacemacs-theme)

;; dired+
(require 'dired+)
(diredp-toggle-find-file-reuse-dir 1)
(define-key dired-mode-map [mouse-2] 'diredp-mouse-find-file-reuse-dir-buffer)

;; company-mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; flx-ido
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; ido-ubiquitous
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; ido-yes-or-no
(require 'ido-yes-or-no)
(ido-yes-or-no-mode 1)

;; smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; projectile
(require 'projectile)
(setq projectile-mode-line "Projectile")
;(setq projectile-keymap-prefix (kbd "C-x p"))
(projectile-global-mode)

;; markdown-mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; anzu
(global-anzu-mode +1)

;; flycheck
(global-flycheck-mode)

;; switch-window
(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)

;; browse-kill-ring+
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; easy-kill
(require 'easy-kill)
(global-set-key [remap kill-ring-save] 'easy-kill)

;; Miscellaneous configuration

;; Line numbering
(require 'linum)
(add-hook 'prog-mode-hook 'linum-on)
(require 'tex)
(add-hook 'LaTeX-mode-hook 'linum-on)
(setq linum-format "%4d ")

;; gdb
(defadvice gdb-setup-windows (around setup-more-gdb-windows activate)
  ad-do-it
  (split-window-horizontally)
  (other-window 1)
  (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-disassembly-buffer)))

;; CEDET
(if (eq system-type 'darwin)
    (progn (setq company-backends (delete 'company-semantic company-backends)))
  (progn (global-ede-mode 1)
         (semantic-mode 1)
         (global-semantic-idle-completions-mode t)
         (global-semantic-decoration-mode t)
         (global-semantic-highlight-func-mode t)
         (global-semantic-show-unmatched-syntax-mode t)))

;; Delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'recentf)
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
(recentf-mode t)
(setq recentf-max-saved-items 15)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

;; Org mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(require 'cc-mode)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
(electric-pair-mode 1)
(setq show-paren-delay 0)
(show-paren-mode 1)
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(setq mac-pass-command-to-system nil)
(if (display-graphic-p) (progn
                          (toggle-scroll-bar -1)
                          (scroll-bar-mode -1)
                          (tool-bar-mode -1)))

;; I use C-q for tmux
(global-unset-key (kbd "C-q"))

;; Custom functions and keybindings
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])
(global-set-key [(control z)] 'undo)
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-set-key (kbd "C-M-d") 'duplicate-line)
(put 'dired-find-alternate-file 'disabled nil)
