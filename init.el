(setq ring-bell-function 'ignore)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 0.3))) ;; one line at a time
    
(setq mouse-wheel-progressive-speed 0.00000001) ;; don't accelerate scrolling
    
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
    
(setq scroll-step 1) ;; keyboard scroll one line at a time

(global-linum-mode t)
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
(setq linum-format "%4d \u2502 ")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; Hookup emacs clipboard with mac system clipboard
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

(load "~/.emacs.d/elpa/auctex-11.89/auctex.el" nil t t)
(add-to-list 'load-path "~/.emacs.d/emacs-neotree")
(require 'neotree)
  (global-set-key (kbd "C-Q") 'neotree-toggle)
;;(load "~/.emacs.d/elpa/auctex-11.89/preview-latex.el" nil t t)
(getenv "PATH")
 (setenv "PATH"
(concat
 "/Library/TeX/texbin" ":" "/usr/local/bin" ":" "/usr/local/MacGPG2/bin" ":" "/usr/local/opt/coreutils/libexec/gnubin"

(getenv "PATH")))
(setq preview-gs-command "/usr/local/bin/gs")
(global-unset-key (kbd "C-w"))
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])
(global-set-key [(control z)] 'undo)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(semantic-mode t)
 '(send-mail-function (quote sendmail-send-it))
 '(user-mail-address "souvik1997@gmail.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
