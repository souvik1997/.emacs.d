;;; packages.el --- Emacs Lisp Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq emacs-lisp-packages
      '(
        auto-compile
        company
        (debug :location built-in)
        (edebug :location built-in)
        eldoc
        elisp-slime-nav
        (emacs-lisp :location built-in)
        evil
        evil-cleverparens
        eval-sexp-fu
        flycheck
        flycheck-package
        ggtags
        counsel-gtags
        helm-gtags
        (ielm :location built-in)
        macrostep
        nameless
        overseer
        parinfer
        semantic
        smartparens
        srefactor
        ))

(defun emacs-lisp/init-ielm ()
  (use-package ielm
    :defer t
    :init
    (progn
      (spacemacs/register-repl 'ielm 'ielm)
      (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
        (spacemacs/declare-prefix-for-mode mode "ms" "ielm")
        (spacemacs/set-leader-keys-for-major-mode mode
          "'" 'ielm
          "si" 'ielm)))
    :config
    (defun ielm-indent-line ()
      (interactive)
      (let ((current-point (point)))
        (save-restriction
          (narrow-to-region (search-backward-regexp "^ELISP>") (goto-char current-point))
          (lisp-indent-line))))))

(defun emacs-lisp/post-init-company ()
  (spacemacs|add-company-backends :backends company-capf
                                  :modes emacs-lisp-mode)
  (spacemacs|add-company-backends :backends (company-files company-capf)
                                  :modes ielm-mode))

(defun emacs-lisp/init-debug ()
  (use-package debug
    :defer t
    :init (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
            (spacemacs/declare-prefix-for-mode mode "md" "debug")
            (spacemacs/set-leader-keys-for-major-mode mode
              "dt" 'spacemacs/elisp-toggle-debug-expr-and-eval-func))
    :config (evilified-state-evilify-map debugger-mode-map
              :mode debugger-mode)))

(defun emacs-lisp/init-edebug ()
  (use-package edebug
    :defer t
    :init
    (progn
      ;; key bindings
      (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
        (spacemacs/set-leader-keys-for-major-mode mode
          "df" 'spacemacs/edebug-instrument-defun-on
          "dF" 'spacemacs/edebug-instrument-defun-off))
      ;; since we evilify `edebug-mode-map' we don't need to intercept it to
      ;; make it work with evil
      (evil-set-custom-state-maps
       'evil-intercept-maps
       'evil-pending-intercept-maps
       'intercept-state
       'evil-make-intercept-map
       (delq (assq 'edebug-mode-map evil-intercept-maps)
             evil-intercept-maps))
      (evilified-state-evilify-map edebug-mode-map
        :eval-after-load edebug
        :bindings
        "a" 'edebug-stop
        "c" 'edebug-go-mode
        "s" 'edebug-step-mode
        "S" 'edebug-next-mode)
      (evilified-state-evilify-map edebug-eval-mode-map
        :eval-after-load edebug
        :bindings
        "a" 'edebug-stop
        "c" 'edebug-go-mode
        "s" 'edebug-step-mode
        "S" 'edebug-next-mode)
      (advice-add 'edebug-mode :after 'spacemacs//edebug-mode))))

(defun emacs-lisp/post-init-eldoc ()
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(defun emacs-lisp/init-auto-compile ()
  (use-package auto-compile
    :defer (spacemacs/defer)
    :init
    (progn
      (spacemacs|require 'auto-compile)
      (setq auto-compile-display-buffer nil
            ;; lets spaceline manage the mode-line
            auto-compile-use-mode-line nil
            auto-compile-mode-line-counter t)
      (add-hook 'emacs-lisp-mode-hook 'auto-compile-mode))
    :config
    (progn
      (spacemacs|hide-lighter auto-compile-mode)
      (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
        "cl" 'auto-compile-display-log))))

(defun emacs-lisp/init-elisp-slime-nav ()
  ;; Elisp go-to-definition with M-. and back again with M-,
  (use-package elisp-slime-nav
    :defer (spacemacs/defer)
    :init
    (progn
      (spacemacs|require 'elisp-slime-nav)
      (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
      (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
        (spacemacs/declare-prefix-for-mode mode "mg" "find-symbol")
        (spacemacs/declare-prefix-for-mode mode "mh" "help")
        (spacemacs/set-leader-keys-for-major-mode mode
          "hh" 'elisp-slime-nav-describe-elisp-thing-at-point)
        (let ((jumpl (intern (format "spacemacs-jump-handlers-%S" mode))))
          (add-to-list jumpl 'elisp-slime-nav-find-elisp-thing-at-point))))
    :config (spacemacs|hide-lighter elisp-slime-nav-mode)

    ))

(defun emacs-lisp/init-emacs-lisp ()
  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
    (spacemacs/declare-prefix-for-mode mode "mc" "compile")
    (spacemacs/declare-prefix-for-mode mode "me" "eval")
    (spacemacs/declare-prefix-for-mode mode "mt" "tests")
    (spacemacs/set-leader-keys-for-major-mode mode
      "cc" 'emacs-lisp-byte-compile
      "e$" 'lisp-state-eval-sexp-end-of-line
      "eb" 'eval-buffer
      "eC" 'spacemacs/eval-current-form
      "ee" 'eval-last-sexp
      "er" 'eval-region
      "ef" 'eval-defun
      "el" 'lisp-state-eval-sexp-end-of-line
      "gG" 'spacemacs/nav-find-elisp-thing-at-point-other-window
      ","  'lisp-state-toggle-lisp-state
      "tb" 'spacemacs/ert-run-tests-buffer
      "tq" 'ert)))

(defun emacs-lisp/init-macrostep ()
  (use-package macrostep
    :defer t
    :mode (("\\*.el\\'" . emacs-lisp-mode)
           ("Cask\\'" . emacs-lisp-mode))
    :init
    (progn
      (evil-define-key 'normal macrostep-keymap "q" 'macrostep-collapse-all)
      (spacemacs|define-transient-state macrostep
        :title "MacroStep Transient State"
        :doc "\n[_e_] expand [_c_] collapse [_n_/_N_] next/previous [_q_] quit"
        :foreign-keys run
        :bindings
        ("e" macrostep-expand)
        ("c" macrostep-collapse)
        ("n" macrostep-next-macro)
        ("N" macrostep-prev-macro)
        ("q" macrostep-collapse-all :exit t))
      (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
        "dm" 'spacemacs/macrostep-transient-state/body))))

(defun emacs-lisp/init-nameless ()
  (use-package nameless
    :defer (spacemacs/defer)
    :init
    (progn
      (spacemacs|require 'nameless)
      (setq
       ;; always show the separator since it can have a semantic purpose
       ;; like in Spacemacs where - is variable and / is a function.
       ;; moreover it makes nameless work for all kind of separators.
       nameless-separator nil
       ;; Use > as the defautl prefix : is already used for
       ;; keywords
       nameless-prefix ">")
      ;; some default aliases for Spacemacs source code
      (setq nameless-global-aliases '(("SB" . "spacemacs-buffer")
                                      ("S"  . "spacemacs")
                                      (".S"  . "dotspacemacs")
                                      ("CL" . "configuration-layer")))
      ;; make `nameless-current-name' safe as a local variable for string values
      (put 'nameless-current-name 'safe-local-variable #'stringp)
      (spacemacs|diminish nameless-mode " 🅽" " [n]")
      (spacemacs|add-toggle nameless
        :status nameless-mode
        :on (nameless-mode)
        :off (nameless-mode -1)
        :evil-leader-for-mode (emacs-lisp-mode . "Tn"))
      ;; activate nameless only when in a GUI
      ;; in a terminal nameless triggers all sorts of graphical glitches.
      (spacemacs|do-after-display-system-init
       (when emacs-lisp-hide-namespace-prefix
         (spacemacs/toggle-nameless-on-register-hook-emacs-lisp-mode))))))

(defun emacs-lisp/init-overseer ()
  (use-package overseer
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
            "ta" 'overseer-test
            "tt" 'overseer-test-run-test
            "tb" 'overseer-test-this-buffer
            "tf" 'overseer-test-file
            "tg" 'overseer-test-tags
            "tp" 'overseer-test-prompt
            "tA" 'overseer-test-debug
            "tq" 'overseer-test-quiet
            "tv" 'overseer-test-verbose
            "th" 'overseer-help)))

(defun emacs-lisp/post-init-evil ()
  (add-hook 'emacs-lisp-mode-hook #'spacemacs//define-elisp-comment-text-object))

(defun emacs-lisp/pre-init-evil-cleverparens ()
  (spacemacs|use-package-add-hook evil-cleverparens
    :pre-init
    (add-to-list 'evil-lisp-safe-structural-editing-modes 'emacs-lisp-mode)))

(defun emacs-lisp/post-init-eval-sexp-fu ()
  (add-hook 'emacs-lisp-mode-hook 'eval-sexp-fu-flash-mode))

(defun emacs-lisp/post-init-flycheck ()
  ;; Don't activate flycheck by default in elisp
  ;; because of too much false warnings
  ;; (spacemacs/enable-flycheck 'emacs-lisp-mode)

  ;; Make flycheck recognize packages in loadpath
  ;; i.e (require 'company) will not give an error now
  (setq flycheck-emacs-lisp-load-path 'inherit))

(defun emacs-lisp/init-flycheck-package ()
  (use-package flycheck-package
    :hook (emacs-lisp-mode . flycheck-package-setup)))

(defun emacs-lisp/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'emacs-lisp-mode))

(defun emacs-lisp/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'emacs-lisp-mode))

(defun emacs-lisp/post-init-ggtags ()
  (add-hook 'emacs-lisp-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun emacs-lisp/post-init-parinfer ()
  (add-hook 'emacs-lisp-mode-hook 'parinfer-mode))

(defun emacs-lisp/post-init-semantic ()
  (add-hook 'emacs-lisp-mode-hook 'semantic-mode)
  (with-eval-after-load 'semantic
    (semantic-default-elisp-setup)))

(defun emacs-lisp/post-init-srefactor ()
  (add-hook 'emacs-lisp-mode-hook 'spacemacs/load-srefactor)
  (use-package srefactor-lisp
    :commands (srefactor-lisp-format-buffer
               srefactor-lisp-format-defun
               srefactor-lisp-format-sexp
               srefactor-lisp-one-line)
    :init
    (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
      (spacemacs/declare-prefix-for-mode mode "m=" "srefactor")
      (spacemacs/set-leader-keys-for-major-mode mode
        "=b" 'srefactor-lisp-format-buffer
        "=d" 'srefactor-lisp-format-defun
        "=o" 'srefactor-lisp-one-line
        "=s" 'srefactor-lisp-format-sexp))))

(defun emacs-lisp/post-init-smartparens ()
  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
    (spacemacs/set-leader-keys-for-major-mode mode
      "ec" 'spacemacs/eval-current-form-sp
      "es" 'spacemacs/eval-current-symbol-sp)))
