; l.el --- a simple major mode for the didactic L programming language
; Author: Benjamin Holder <benjamin.holder@utexas.edu>
; This software is made available under the terms of the GNU General Public License version 3 or any later version. You may freely modify and distribute it provided this notice is left intact.

; To automatically load this file on starting Emacs write
; (load "<path>l.el") in your .emacs file, where <path> is the path to this file

; Automatically recognize .L
(setq auto-mode-alist
  (append '(("\\.L\\'" . l-mode)) auto-mode-alist)
)

(defvar l-mode-keywords
   (regexp-opt '("else" "fun" "let" "if" "in" "isnil" "print" "readint" "readstring" "then" "with") 'words)
)

; Highlight keywords, constants, function definitions, variable definitions
(defvar l-mode-font-lock-keywords
  '(
    (eval . l-mode-keywords)
    ("\\<\\(lambda\\|nil\\|[[:digit:]]+\\)\\>" . font-lock-constant-face)
    ("\\<fun\\s-+\\([a-zA-Z_]\\w*\\)\\s-+with\\>" . (1 font-lock-function-name-face))
    ("\\<let\\s-+\\([a-zA-Z_]\\w*\\)\\s-*=" . (1 font-lock-variable-name-face))
   )
)

; Originally, l-mode derived from prog-mode but derives from fundamental-mode
; because some Emacs versions it needs to run on apparently lack it.
(define-derived-mode l-mode fundamental-mode "L"
  "Major mode for the didactic L programming language.
\\<l-mode-mode-map>
Keybindings:
\\[compile]\tcompile
\\[recompile]\trecompile
\\[comment-region]\tcomment-region
\\[uncomment-region]\tuncomment-region"

; Comment Variables
  (setq comment-column 0)
  (setq comment-start "(*")
  (setq comment-end "*)")
  (setq comment-start-skip "(\\*")

  (setq font-lock-defaults
	'(l-mode-font-lock-keywords nil t))
  (use-local-map l-mode-mode-map)
)

; Modify the generated syntax table
(modify-syntax-entry ?_  "w"     l-mode-syntax-table)

(modify-syntax-entry ?+  "."     l-mode-syntax-table)
(modify-syntax-entry ?-  "."     l-mode-syntax-table)
(modify-syntax-entry ?*  ". 23n" l-mode-syntax-table)
(modify-syntax-entry ?/  "."     l-mode-syntax-table)
(modify-syntax-entry ?&  "."     l-mode-syntax-table)
(modify-syntax-entry ?|  "."     l-mode-syntax-table)
(modify-syntax-entry ?=  "."     l-mode-syntax-table)
(modify-syntax-entry ?<  "."     l-mode-syntax-table)
(modify-syntax-entry ?>  "."     l-mode-syntax-table)

(modify-syntax-entry ?\( "()1"   l-mode-syntax-table)
(modify-syntax-entry ?\) ")(4"   l-mode-syntax-table)

; L mode keymap
(setq l-mode-mode-map (make-sparse-keymap))

(define-key l-mode-mode-map "\C-cc"    'compile)
(define-key l-mode-mode-map "\C-cr"    'recompile)
(define-key l-mode-mode-map "\C-c\C-c" 'comment-region)
(define-key l-mode-mode-map "\C-c\C-u" 'uncomment-region)
