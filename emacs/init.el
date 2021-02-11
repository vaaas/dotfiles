;; -*- lexical-binding: t; -*-
(require 'package)
(require 'iso-transl)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(electric-indent-mode -1)
(ido-mode 1)
(global-visual-line-mode 1)
(abbrev-mode 1)

(setq-default indent-tabs-mode nil
	line-spacing 0.3
	tab-width 4
	mode-line-format nil
	save-abbrevs nil)
(setq inhibit-splash-screen t
	inhibit-startup-message t
	frame-resize-pixelwise t
	vc-follow-symlinks t
	make-backup-files nil
	blog-directory "/home/vas/Projects/website"
	disabled-command-function nil)
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.blade.php\\'" . js-mode))

(defun backward-whitespace() (interactive) (forward-whitespace -1))

(defun zap-up-to-char-backward() (interactive)
	(zap-up-to-char -1 (read-char "zap to char backward")))

(defun goto-char() (interactive)
	(search-forward (make-string 1 (read-char "go to char"))))

(defun goto-char-backward() (interactive)
	(search-backward (make-string 1 (read-char "go to char backward"))))

(defun insert-indent()
	(if indent-tabs-mode (insert-char 9) (insert-char 32 tab-width)))

(defun delete-indent()
	(cond
		((and indent-tabs-mode (= 9 (char-after)))
			(delete-forward-char 1))
		((and
			(not indent-tabs-mode)
			(string=
				(make-string tab-width 32)
				(buffer-substring (point) (+ (point) tab-width))))
			(delete-forward-char tab-width))
		(t (do-nothing))))

(defun expand-or-tab() (interactive)
	(if (member (char-before) '(9 10 32))
		(insert-indent)
		(call-interactively 'dabbrev-expand)))

(defun line-below() (interactive)
	(end-of-line) (newline-and-indent-relative))

(defun line-above() (interactive)
	(save-excursion
		(beginning-of-line)
		(newline)
		(forward-line -1)
		(indent-relative)))

(defun newline-and-indent-relative() (interactive) (newline) (indent-relative t t))

(defun do-nothing() (interactive))
(put 'do-nothing 'no-self-insert t)

(defun add-trailing-newline() (end-of-buffer) (when (not (= 10 (char-before))) (insert-char 10)))

(defun add-trailing-newline() (end-of-buffer) (when (not (= 10 (char-before))) (insert-char 10)))

(defun quick-find-file() (interactive)
	(find-file
	(ido-completing-read "select file> "
	(split-string
	(shell-command-to-string "xzcat ~/filedb.xz") "\n"))))

(defun dired-here() (interactive) (dired default-directory))

(defun vi-on() (interactive)
	(vi-mode 1)
	(setq cursor-type 'box))

(defun vi-off() (interactive)
	(vi-mode -1)
	(setq cursor-type 'bar))

(defun backspace-or-unindent() (interactive)
	(cond
		((use-region-p) (call-interactively 'kill-region))
		((< (point) (+ 1 tab-width)) (backward-delete-char 1))
		(indent-tabs-mode (backward-delete-char 1))
		((string= (make-string tab-width 32) (buffer-substring (point) (- (point) tab-width)))
			(backward-delete-char tab-width))
		(t (backward-delete-char 1))))

(defun double-newline() (interactive)
	(cond
		((= 0 (point)) (newline))
		((= 10 (char-before)) (newline))
		(t (newline) (newline))))

(defun toggle-indent-tabs() (interactive)
	(if indent-tabs-mode
		(progn (setq indent-tabs-mode nil) (setq tab-width 4) (message "indent will use SPACES"))
		(progn (setq indent-tabs-mode t) (setq tab-width 3) (message "indent will use TABS"))))

(defun space-comma-dot() (interactive)
	(cond
		((< (point) 3) (self-insert-command 1))
		((string= ", " (buffer-substring (point) (- (point) 2)))
			(progn (backward-delete-char 2) (insert ". ")))
		((= 32 (char-before (point)))
			(progn (backward-delete-char 1) (insert ", ")))
		(t (self-insert-command 1))))

(defun replace-all (from to)
	(beginning-of-buffer)
	(while (search-forward from nil t)
		(replace-match to t t)))

(defun replace-all-regex (from to)
	(beginning-of-buffer)
	(while (re-search-forward from nil t)
		(replace-match to t nil)))

(defun french() (interactive)
	(replace-all-regex "\"\\([^\"]+?\\)\"" "«\\1»")
	(replace-all "?!" "⁈")
	(replace-all-regex "[ |\u00A0|\u202F]*\\(;\\|!\\|\\?\\|⁈\\)" "\u202F\\1")
	(replace-all-regex "[ |\u00A0|\u202F]*\\([:\\|»]\\)" "\u00A0\\1")
	(replace-all-regex "«[ |\u00A0|\u202F]*" "«\u00A0"))

(defun shell-command-this-buffer(x) (interactive)
	(shell-command-on-region (point-min) (point-max) x (current-buffer) t))

(defun cmark() (interactive)
	(shell-command-this-buffer "cmark --smart --unsafe"))

(defun spaces-to-tabs() (interactive)
	(beginning-of-buffer)
	(replace-all (make-string tab-width 32) "	"))

(defun join-line() (interactive)
	(end-of-line)
	(forward-line)
	(beginning-of-line-text)
	(delete-indentation))

(defun indent-line-or-region() (interactive)
	(if (use-region-p)
		(progn
			(call-interactively 'indent-rigidly-right-to-tab-stop)
			(setq deactivate-mark nil))
		(save-excursion (beginning-of-line) (insert-indent))))

(defun unindent-line-or-region() (interactive)
	(if (use-region-p)
		(progn
			(call-interactively 'indent-rigidly-left-to-tab-stop)
			(setq deactivate-mark nil))
		(save-excursion (beginning-of-line) (delete-indent))))

(defun timestamp() (format-time-string "%s"))

(defun blog() (interactive)
	(setq cat (ido-completing-read "category?> "
		'("tech" "anime" "books" "memes" "films" "journal" "games")))
	(setq stamp (timestamp))
	(delete-trailing-whitespace)
	(beginning-of-buffer)
	(if (< (buffer-size) 2000)
		(insert (format "\n%s\n%s\n" stamp cat))
	(progn
		(search-forward "<h1>") (setq start (point))
		(search-forward "</h1>") (setq end (- (point) 5))
		(setq title (buffer-substring start end))
		(search-forward "<p>") (setq start (point))
		(search-forward "</p>") (setq end (- (point) 4))
		(setq blurb (buffer-substring start end))
		(setq file-name (concat
			(replace-regexp-in-string " " "_"
				(read-string "file name (no extension): "))
			".html"))
		(shell-command-this-buffer (concat blog-directory "/ncrender -s"))
		(append-to-file (point-min) (point-max) (concat blog-directory "/render/" cat "/" file-name))
		(kill-region (point-min) (point-max))
		(insert
			(format "\n%s\n%s\n<h1><a href='/%s/%s'>%s</a></h1>\n%s"
			stamp cat cat file-name title blurb))
	(add-trailing-newline)
	(append-to-file (point-min) (point-max) (concat blog-directory "/posts")))))

(setq vi-mode-map (make-sparse-keymap))
(define-key vi-mode-map (kbd "k") 'kmacro-start-macro)
(define-key vi-mode-map (kbd "K") 'kmacro-end-or-call-macro)
(define-key vi-mode-map (kbd "c") 'kill-whole-line)
(define-key vi-mode-map (kbd "C") 'kill-line)
(define-key vi-mode-map (kbd "d") 'backward-kill-word)
(define-key vi-mode-map (kbd "D") 'kill-word)
(define-key vi-mode-map (kbd "m") 'indent-line-or-region)
(define-key vi-mode-map (kbd "M") 'unindent-line-or-region)
(define-key vi-mode-map (kbd "j") 'join-line)
(define-key vi-mode-map (kbd "q") 'kill-this-buffer)
(define-key vi-mode-map (kbd "w") 'forward-word)
(define-key vi-mode-map (kbd "W") 'forward-whitespace)
(define-key vi-mode-map (kbd "u") 'backward-word)
(define-key vi-mode-map (kbd "U") 'backward-whitespace)
(define-key vi-mode-map (kbd "v") 'goto-line)
(define-key vi-mode-map (kbd ";") (lambda() (interactive) (end-of-line) (insert-char #x3B)))
(define-key vi-mode-map (kbd ":") 'eval-expression)
(define-key vi-mode-map (kbd "`") 'eshell)

(define-key vi-mode-map (kbd "r") 'zap-up-to-char)
(define-key vi-mode-map (kbd "R") 'zap-up-to-char-backward)
(define-key vi-mode-map (kbd "s") (lambda() (interactive) (if (use-region-p) (deactivate-mark) (call-interactively 'set-mark-command))))
(define-key vi-mode-map (kbd "S") (lambda() (interactive) (beginning-of-line) (call-interactively 'set-mark-command) (forward-line)))
(define-key vi-mode-map (kbd "t") 'goto-char)
(define-key vi-mode-map (kbd "T") 'goto-char-backward)
(define-key vi-mode-map (kbd "n") 'backspace-or-unindent)
(define-key vi-mode-map (kbd "N") 'delete-forward-char)
(define-key vi-mode-map (kbd "h") 'scroll-up-command)
(define-key vi-mode-map (kbd "H") 'scroll-down-command)
(define-key vi-mode-map (kbd "y") 'isearch-repeat-forward)
(define-key vi-mode-map (kbd "Y") 'isearch-repeat-backward)
(define-key vi-mode-map (kbd "i") 'left-char)
(define-key vi-mode-map (kbd "I") 'beginning-of-line-text)
(define-key vi-mode-map (kbd "e") 'next-line)
(define-key vi-mode-map (kbd "E") 'forward-paragraph)
(define-key vi-mode-map (kbd "o") 'previous-line)
(define-key vi-mode-map (kbd "O") 'backward-paragraph)
(define-key vi-mode-map (kbd "a") 'right-char)
(define-key vi-mode-map (kbd "A") 'end-of-line)

(define-key vi-mode-map (kbd "' v p") 'variable-pitch-mode)
(define-key vi-mode-map (kbd "' e a") (lambda() (interactive) (find-file "~/.config/emacs/abbrev_defs")))
(define-key vi-mode-map (kbd "' e i") (lambda() (interactive) (find-file "~/.config/emacs/init.el")))
(define-key vi-mode-map (kbd "' i t") 'toggle-indent-tabs)
(define-key vi-mode-map (kbd "' m m") 'markdown-mode)
(define-key vi-mode-map (kbd "' f r") 'french)
(define-key vi-mode-map (kbd "' c m") 'cmark)
(define-key vi-mode-map (kbd "' s t") 'spaces-to-tabs)
(define-key vi-mode-map (kbd "' e r") 'eval-region)
(define-key vi-mode-map (kbd "' s h") 'split-window-horizontally)
(define-key vi-mode-map (kbd "' s v") 'split-window-vertically)
(define-key vi-mode-map (kbd "' s s") 'delete-other-windows)
(define-key vi-mode-map (kbd "' i b") 'ibuffer)

(define-key vi-mode-map (kbd "z") 'undo)
(define-key vi-mode-map (kbd "Z") 'keyboard-quit)
(define-key vi-mode-map (kbd "f") 'find-file)
(define-key vi-mode-map (kbd "F") 'quick-find-file)
(define-key vi-mode-map (kbd "g") 'beginning-of-buffer)
(define-key vi-mode-map (kbd "G") 'end-of-buffer)
(define-key vi-mode-map (kbd "l") 'line-below)
(define-key vi-mode-map (kbd "L") 'line-above)
(define-key vi-mode-map (kbd "x") 'execute-extended-command)
(define-key vi-mode-map (kbd "b") 'ido-switch-buffer)
(define-key vi-mode-map (kbd "B") 'save-buffer)
(define-key vi-mode-map (kbd "p") 'yank)
(define-key vi-mode-map (kbd "P") 'kill-ring-save)
(define-key vi-mode-map (kbd ".") 'repeat)
(define-key vi-mode-map (kbd "/") 'isearch-forward)
(define-key vi-mode-map (kbd "?") 'isearch-backward)

(define-key vi-mode-map (kbd "SPC") 'vi-off)
(define-key vi-mode-map (kbd "<escape>") 'keyboard-quit)
(define-key vi-mode-map (kbd "(") 'previous-buffer)
(define-key vi-mode-map (kbd ")") 'next-buffer)

(define-minor-mode vi-mode
	"Ghetto vi mode"
	:lighter " vi"
	:keymap 'vi-mode-map)

(define-key global-map (kbd "C-r") 'expand-or-tab)
(define-key global-map (kbd "C-n") 'newline-and-indent-relative)
(define-key global-map (kbd "C-s") 'backspace-or-unindent)
(define-key global-map (kbd "C-t") 'backward-kill-word)
(define-key global-map (kbd "C-g") 'vi-on)
(define-key global-map (kbd "<C-tab>") 'other-window)
(define-key global-map (kbd "C-,") 'previous-buffer)
(define-key global-map (kbd "C-.") 'next-buffer)
(define-key global-map (kbd "<backspace>") 'backspace-or-unindent)
(define-key global-map (kbd "<tab>") 'expand-or-tab)
(define-key global-map (kbd "<escape>") 'vi-on)
(define-key global-map (kbd "<f2>") 'dired-here)

(define-key prog-mode-map (kbd "<return>") 'newline-and-indent-relative)
(define-key prog-mode-map (kbd "C-i") 'newline-and-indent-relative)
(define-key prog-mode-map (kbd "C-SPC") 'unexpand-abbrev)

(define-key text-mode-map (kbd "C-SPC") 'unexpand-abbrev)
(define-key text-mode-map (kbd "SPC") 'space-comma-dot)

(define-key minibuffer-local-map (kbd "<escape>") 'abort-recursive-edit)
(define-key minibuffer-local-map (kbd "<tab>") 'minibuffer-complete)

(define-key isearch-mode-map (kbd "<return>") 'isearch-exit)
(define-key isearch-mode-map (kbd "<escape>") 'isearch-exit)
(define-key isearch-mode-map (kbd "C-g") 'isearch-exit)

(add-hook 'prog-mode-hook (lambda() (abbrev-mode 1) (vi-on)))
(add-hook 'minibuffer-setup-hook 'vi-off)
(add-hook 'text-mode-hook (lambda() (abbrev-mode 1) (variable-pitch-mode) (vi-on)))
(add-hook 'eshell-mode-hook 'vi-off)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'ido-minibuffer-setup-hook (lambda()
	(define-key ido-completion-map (kbd "C-e") 'ido-next-match)
	(define-key ido-completion-map (kbd "C-o") 'ido-prev-match)))

(with-eval-after-load 'markdown-mode
	(define-key markdown-mode-map (kbd "<return>") 'double-newline)
	(define-key markdown-mode-map (kbd "C-n") 'double-newline))

(with-eval-after-load 'dired
	(define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)
	(define-key dired-mode-map (kbd "a") 'dired-find-alternate-file)
	(define-key dired-mode-map (kbd "e") 'dired-next-line)
	(define-key dired-mode-map (kbd "o") 'dired-previous-line)
	(define-key dired-mode-map (kbd "i") 'dired-up-directory)
	(define-key dired-mode-map (kbd "/") 'isearch-exit-forward))

(with-eval-after-load 'php-mode
	(define-key php-mode-map (kbd "<tab>") nil))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (restclient markdown-mode php-mode))))
(custom-set-faces
	'(default ((t (:inherit nil :stipple nil :background "#ffeedd" :foreground "#000000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "1ASC" :family "Liberation Mono"))))
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
	'(isearch ((t (:background "#2255aa" :foreground "#ffffee"))))
	'(lazy-highlight ((t (:inherit highlight))))
	'(minibuffer-prompt ((t (:foreground "#ff0055"))))
	'(mmm-default-submode-face ((t nil)))
	'(mode-line ((t (:background "#ddddcc"))))
	'(mode-line-inactive ((t nil)))
	'(php-$this ((t (:slant oblique))))
	'(php-$this-sigil ((t (:inherit php-$this))))
	'(php-function-call ((t (:inherit font-lock-function-name-face))))
	'(show-paren-match ((t (:inherit highlight))))
	'(variable-pitch ((t (:height 240 :family "Sans Serif")))))
