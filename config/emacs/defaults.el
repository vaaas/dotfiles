(when (display-graphic-p)
	(scroll-bar-mode -1)
	(tool-bar-mode -1))

(menu-bar-mode -1)
(electric-indent-mode -1)
(ido-mode 1)
(global-visual-line-mode 1)
(prefer-coding-system 'utf-8-unix)

(setq-default
	indent-tabs-mode nil
	line-spacing 0.3
	tab-width 4
	mode-line-format nil
	version-control "never"
	create-lockfiles nil
	eval-process "cat"
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
	save-abbrevs nil
	auto-save-default nil
	tty (getenv "XDG_VTNR")
	inhibit-splash-screen t
	inhibit-startup-message t
	frame-resize-pixelwise t
	vc-follow-symlinks t
	make-backup-files nil
	blog-directory (expand-file-name "~/Projects/website")
	disabled-command-function nil
	file-db (expand-file-name "~/filedb.txt")
	file-db-root-dir "~/Projects"
	file-db-exclude-dirs '("." ".." "node_modules" ".git" "public" "vendor" "build")
	blog-categories '("tech" "anime" "books" "memes" "films" "journal" "games")
	custom-file (concat user-emacs-directory "defaults.el"))
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.blade.php\\'" . js-mode))

(custom-set-variables
	'(package-selected-packages '(php-mode)))
(custom-set-faces
	(list 'default (list (list t (list :inherit nil :stipple nil :background (if (display-graphic-p) "#ffeedd" nil) :foreground "#000000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant 'normal :weight 'normal :height 140 :width 'normal :family "JuliaMono"))))
	'(cursor ((t (:background "#ff0055"))))
	'(eshell-ls-directory ((t (:foreground "#2255aa" :weight bold))))
	'(eshell-ls-executable ((t (:foreground "#008844" :weight bold))))
	'(eshell-prompt ((t (:foreground "#ff0055" :weight bold))))
	'(font-lock-builtin-face ((t (:underline (:color foreground-color)))))
	'(font-lock-comment-face ((t (:foreground "#aa4422"))))
	'(font-lock-type-face ((t (:foreground "#d33682"))))
	'(font-lock-constant-face ((t (:inherit font-lock-type-face))))
	'(font-lock-function-name-face ((t (:foreground "#2255aa" :weight bold))))
	'(font-lock-keyword-face ((t (:weight semi-bold))))
	'(font-lock-string-face ((t (:foreground "#008844"))))
	'(font-lock-type-face ((t :foreground "#884488")))
	'(font-lock-variable-name-face ((t nil)))
	'(fringe ((t nil)))
	'(cursor ((t (:background "#ff0055"))))
	'(region ((t (:background "#ffffff"))))
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
	'(show-paren-match ((t (:inherit highlight)))))
