(setq ring-bell-function 'ignore)

;; Mouse configuration
(setq mouse-wheel-scroll-amount '(2 ((shift) . 0.6))) ;; one line at a time
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

;; Line numbering
(global-linum-mode t)
(setq linum-format "%4d \u2502 ")

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
(setq insert-directory-program "/usr/local/bin/gls")
(setq dired-listing-switches "-aBhl --group-directories-first")
(getenv "PATH")
 (setenv "PATH"
(concat
 "/Library/TeX/texbin" ":" "/usr/local/bin/" ":" "/usr/local/MacGPG2/bin" ":" "/usr/local/opt/coreutils/libexec/gnubin"
(getenv "PATH")))
(add-to-list 'exec-path "/Library/TeX/texbin")
(add-to-list 'exec-path  "/usr/local/bin")
(add-to-list 'exec-path "/usr/local/MacGPG2/bin")
(add-to-list 'exec-path "/usr/local/opt/coreutils/libexec/gnubin")

;; Package-specific configuration
;; Package archives
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
;; From http://y.tsutsumi.io/emacs-from-scratch-part-2-package-management.html
(defvar required-packages
  '(auctex
    neotree
    magit
    autopair
    smooth-scrolling
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
  (global-set-key (kbd "C-Q") 'neotree-toggle)

;; Autopair
(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)

;; Smooth-scrolling
(require 'smooth-scrolling)

;; Miscellaneous configuration
(require 'cc-mode)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;; Custom functions and keybindings
(global-unset-key (kbd "C-w"))
(global-set-key (kbd "C-w") 'backward-kill-word)
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



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(inhibit-startup-screen t)
 '(semantic-mode t)
 '(send-mail-function (quote sendmail-send-it))
 '(user-mail-address "souvik1997@gmail.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
