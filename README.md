# Install

`git clone https://github.com/souvik1997/.emacs.d.git ~/.emacs.d`

When Emacs starts it will automatically install all required packages. Emacs versions 25.1 and newer should work.

# Notes

* `company-mode` is disabled for `gud` because it causes Emacs to freeze after running any program with `gdb`.
* On OS X, CEDET's `semantic-mode` is disabled because of bugs with `gdb-many-windows`. On Linux `semantic-mode` is still enabled.
* `markdown-mode` expects `markdown` to be installed.
* On OS X, `/usr/local/bin/gls` is used for `dired-mode`. This can be installed with `brew install coreutils`.
* TRAMP does not seem to work correctly if non-standard shells (like `fish`) are used.
* On OS X (maybe some Linux distros as well), `C-SPC` is mapped to switching input language. You will have to disable this keyboard shortcut for using `C-SPC` in Emacs

# Todo
Find some way to integrate the X11 clipboard with Emacs in terminal mode
