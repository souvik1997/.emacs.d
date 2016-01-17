;; Package-specific configuration
;; Package archives
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(company-begin-commands
   (quote
    (self-insert-command org-self-insert-command orgtbl-self-insert-command)))
 '(company-global-modes (quote (not gud-mode)))
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 1)
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes t)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js2-idle-timer-delay 2)
 '(package-selected-packages
   (quote
    (spaceline spacemacs-theme markdown-mode visible-mark company-math company-web company-auctex company smex ido-yes-or-no ido-ubiquitous flx-ido projectile dired+ web-mode js2-mode smooth-scrolling magit neotree auctex)))
 '(powerline-default-separator (quote wave))
 '(powerline-height 23)
 '(projectile-keymap-prefix "p")
 '(send-mail-function (quote sendmail-send-it))
 '(user-mail-address "souvik1997@gmail.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(term-color-black ((t (:background "dark gray" :foreground "gray"))))
 '(term-color-blue ((t (:background "deep sky blue" :foreground "cyan")))))

(package-initialize)
(require 'server)
(unless (server-running-p) (server-start))

(setq ring-bell-function 'ignore)

;; Mouse configuration
(setq mouse-wheel-scroll-amount '(3 ((shift) . 0.6)))
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse


(unless window-system
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
  (setq mouse-sel-mode t))

;; TRAMP mode
(setq tramp-default-method "ssh")



;; Backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Hook up emacs clipboard with mac system clipboard
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

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
 "/Library/TeX/texbin" ":" "/usr/local/bin/" ":" "/usr/local/MacGPG2/bin" ":" "/usr/local/opt/coreutils/libexec/gnubin" ":" "/usr/bin/" ":"
(getenv "PATH")))
(add-to-list 'exec-path "/usr/bin/")
(add-to-list 'exec-path "/Library/TeX/texbin")
(add-to-list 'exec-path  "/usr/local/bin")
(add-to-list 'exec-path "/usr/local/MacGPG2/bin")
(add-to-list 'exec-path "/usr/local/opt/coreutils/libexec/gnubin")


;; From http://y.tsutsumi.io/emacs-from-scratch-part-2-package-management.html
(defvar required-packages
  '(auctex
    neotree
    magit
    smooth-scrolling
    js2-mode
    web-mode
    dired+
    projectile
    flx-ido
    ido-ubiquitous
    ido-yes-or-no
    smex
    company
    company-auctex
    company-web
    company-math
    visible-mark
    markdown-mode
    spacemacs-theme
    spaceline
  )
  "List of packages that must be installed")
(require 'cl)
(defun packages-installed-p ()
  (loop for p in required-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))
(unless (packages-installed-p)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))



;; AUCTeX
;(load "auctex.el" nil t t)
;(load "preview-latex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq preview-gs-command "/usr/local/bin/gs")
(setq LaTeX-item-indent 0)

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
(setq recentf-max-saved-items 50)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

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
