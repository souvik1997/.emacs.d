(load "term/xterm")
(defun terminal-init-screen ()
  "Terminal initialization function for screen-256color."
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))
