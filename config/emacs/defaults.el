; -*- lexical-binding: t -*-

; disable the scroll bar, tool bar, and other widgets that get in the way
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

; by default, emacs does various magic things with automatic indentation. I override this with custom indentation functions, so disable electric indent
(electric-indent-mode -1)

; enable ido-mode, which looks like fzf
(ido-mode 1)

; enable visual line mode in all buffers, which soft wraps long lines
(global-visual-line-mode 1)

; a nice default system encoding. :)
(prefer-coding-system 'utf-8-unix)

(setq-default
	; we use spaces at work by default. but at home I prefer tabs
	indent-tabs-mode (when (string= (system-name) "cleome") t)

	; this is the line-height
	line-spacing 0.3

	; 3 spaces for tabs. 4 spaces for spaces.
	tab-width (if (string= (system-name) "cleome") 3 4)

	; I don't use the mode-line, so don't display it
	mode-line-format nil

	; emacs creates automatic copies/versions and lockfiles for every visited file. this makes a mess of the file system, so disable it
	version-control "never"
	create-lockfiles nil
	auto-save-default nil
	make-backup-files nil

	; common default symbols for php and js
	prettify-symbols-alist '(
		("=>" . ?⇒)
		(">=" . ?≥)
		("<=" . ?≤)
		("==" . ?=)
		("===" . ?≡)
		("!=" . ?≠)
		("!==" . ?≢)
		("=" . ?≔)
		("..." . ?…)
		("." . ?·)
		("->" . ?→)
		("::" . ?︙)
		("!" ?¬)
		("&&" ?∧)
		("||" ?∨)
		(">>" ?≫)
		("|>" ?▷))

	; emacs automatically increments the abbrevs file usage so that the user can find unused abbrevs. this messes version control of the abbrevs file, so disable it
	save-abbrevs nil

	; I don't want a splash screen or startup message
	inhibit-splash-screen t
	inhibit-startup-message t

	; by default, emacs resizes the window in discreet increments of the font size. this allows the window to resize on a pixel by pixel basis, which is the default behaviour for most windows.
	frame-resize-pixelwise t

	; if a file is a symlink, edit the file it points to instead of the symlink itself.
	vc-follow-symlinks t

	; by default, emacs disables some "advanced" commands. this is annoying, so disable it.
	disabled-command-function nil

	; we want custom-variables and custom faces to be stored in THIS file. otherwise they are stored in init.el
	custom-file (concat user-emacs-directory "defaults.el")

	; during isearch, turn every whitespace into a glob star. that way "d e p" can match "defvar eval-process"
	search-whitespace-regexp ".*?"

	; enable "fuzzy search" in ido
	ido-enable-flex-matching t

	; these are the installed packages from MELPA
	package-selected-packages '(php-mode markdown-mode))

(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.blade.php\\'" . php-mode))

(custom-set-faces
	;; custom-set-faces was added by Custom.
	;; If you edit it by hand, you could mess it up, so be careful.
	;; Your init file should contain only one such instance.
	;; If there is more than one, they won't work right.
	'(default ((t (:inherit nil :extend nil :stipple nil :background "#ffeedd" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :family "JuliaMono"))))
	'(cursor ((t (:background "#ff0055"))))
	'(eshell-ls-directory ((t (:foreground "#2255aa" :weight bold))))
	'(eshell-ls-executable ((t (:foreground "#008844" :weight bold))))
	'(eshell-prompt ((t (:foreground "#ff0055" :weight bold))))
	'(font-lock-builtin-face ((t (:underline (:color foreground-color)))))
	'(font-lock-comment-face ((t (:foreground "#aa4422"))))
	'(font-lock-constant-face ((t (:inherit font-lock-type-face))))
	'(font-lock-function-name-face ((t (:foreground "#2255aa" :weight bold))))
	'(font-lock-keyword-face ((t (:weight semi-bold))))
	'(font-lock-string-face ((t (:foreground "#008844"))))
	'(font-lock-type-face ((t (:foreground "#d33682"))))
	'(font-lock-variable-name-face ((t nil)))
	'(fringe ((t nil)))
	'(highlight ((t (:background "#ccccff"))))
	'(isearch ((t (:background "#2255aa" :foreground "#ffeedd"))))
	'(lazy-highlight ((t (:inherit highlight))))
	'(minibuffer-prompt ((t (:foreground "#ff0055"))))
	'(mmm-default-submode-face ((t nil)))
	'(mode-line ((t (:background "#ddddcc"))))
	'(mode-line-inactive ((t nil)))
	'(php-$this ((t (:slant oblique))))
	'(php-$this-sigil ((t (:inherit php-$this))))
	'(php-function-call ((t (:inherit font-lock-function-name-face))))
	'(region ((t (:background "#ffffff"))))
	'(show-paren-match ((t (:inherit highlight)))))
