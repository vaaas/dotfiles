(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(electric-indent-mode -1)
(ido-mode 1)
(global-visual-line-mode 1)
(prefer-coding-system 'utf-8-unix)

(setq-default
	indent-tabs-mode nil
	line-spacing 0.3
	tab-width 4
	mode-line-format nil
	save-abbrevs nil
	auto-save-default nil
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
		("|>" ?▷)))
(setq
	inhibit-splash-screen t
	inhibit-startup-message t
	frame-resize-pixelwise t
	vc-follow-symlinks t
	make-backup-files nil
	blog-directory "/home/vas/Projects/website"
	disabled-command-function nil
	file-db (expand-file-name "~/filedb.txt"))
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.blade.php\\'" . js-mode))
