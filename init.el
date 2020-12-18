;; -*- lexical-binding: t; -*-
(require 'package)
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
	save-abbrevs nil)
(setq inhibit-splash-screen t
	inhibit-startup-message t
	frame-resize-pixelwise t
	vc-follow-symlinks t
	make-backup-files nil)
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))

(put 'dired-find-alternate-file 'disabled nil)

(defun expand-or-tab() (interactive)
	(if (member (char-before) '(9 10 32))
		(if indent-tabs-mode (insert-char 9) (insert-char 32 tab-width))
		(call-interactively 'dabbrev-expand)))

(defun line-below() (interactive)
	(end-of-line) (open-line 1) (forward-line))

(defun newline-and-indent-relative() (interactive) (newline) (indent-relative t t))

(defun do-nothing() (interactive))

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
		((string= (make-string tab-width ? ) (buffer-substring (point) (- (point) tab-width)))
			(backward-delete-char tab-width))
		(t (backward-delete-char 1))))

(defun double-newline() (interactive)
	(if (= 10 (char-before))
		(newline)
		(progn (newline) (newline))))

(defun toggle-indent-tabs() (interactive)
	(if indent-tabs-mode
		(progn (setq indent-tabs-mode nil) (message "indent will use SPACES"))
		(progn (setq indent-tabs-mode t) (message "indent will use TABS"))))

(defun space-comma-dot() (interactive)
	(cond
		((< (point) 3) (self-insert-command 1))
		((string= ", " (buffer-substring (point) (- (point) 2)))
			(progn (backward-delete-char 2) (insert ". ")))
		((= 32 (char-before (point)))
			(progn (backward-delete-char 1) (insert ", ")))
		(t (self-insert-command 1))))

(defun replace-all (from to)
	(goto-char (point-min))
	(while (search-forward from nil t)
		(replace-match to nil t)))

(defun replace-all-regex (from to)
	(goto-char (point-min))
	(while (re-search-forward from nil t)
		(replace-match to nil nil)))

(defun french() (interactive)
	(replace-all-regex "\"\\([^\"]+?\\)\"" "«\\1»")
	(replace-all "?!" "⁈")
	(replace-all-regex "[ |\u00A0|\u202F]*\\(;\\|!\\|\\?\\|⁈\\)" "\u202F\\1")
	(replace-all-regex "[ |\u00A0|\u202F]*\\([:\\|»]\\)" "\u00A0\\1")
	(replace-all-regex "«[ |\u00A0|\u202F]*" "«\u00A0"))

(defun cmark() (interactive)
	(shell-command-on-region (point-min) (point-max)
		"cmark --smart --unsafe"
		(current-buffer) t))

(defun spaces-to-tabs() (interactive)
	(goto-char (point-min))
	(replace-all (make-string tab-width ? ) "	"))

(defun join-line() (interactive)
	(end-of-line)
	(forward-line)
	(beginning-of-line-text)
	(delete-indentation))

(setq vi-mode-map (make-sparse-keymap))
(define-key vi-mode-map (kbd "k") 'kmacro-start-macro)
(define-key vi-mode-map (kbd "K") 'kmacro-end-or-call-macro)
(define-key vi-mode-map (kbd "i") 'left-char)
(define-key vi-mode-map (kbd "e") 'next-line)
(define-key vi-mode-map (kbd "o") 'previous-line)
(define-key vi-mode-map (kbd "a") 'right-char)
(define-key vi-mode-map (kbd "I") 'beginning-of-line-text)
(define-key vi-mode-map (kbd "E") 'forward-paragraph)
(define-key vi-mode-map (kbd "O") 'backward-paragraph)
(define-key vi-mode-map (kbd "A") 'end-of-line)
(define-key vi-mode-map (kbd "w") 'forward-to-word)
(define-key vi-mode-map (kbd "u") 'backward-word)
(define-key vi-mode-map (kbd "r") 'zap-to-char)
(define-key vi-mode-map (kbd "t") 'delete-forward-char)
(define-key vi-mode-map (kbd "c") 'kill-whole-line)
(define-key vi-mode-map (kbd "C") 'kill-line)
(define-key vi-mode-map (kbd "d") 'kill-word)
(define-key vi-mode-map (kbd "n") 'backspace-or-unindent)
(define-key vi-mode-map (kbd "m") 'backward-kill-word)
(define-key vi-mode-map (kbd ".") 'repeat)
(define-key vi-mode-map (kbd "j") 'join-line)
(define-key vi-mode-map (kbd "s") (lambda() (interactive) (if (use-region-p) (deactivate-mark) (call-interactively 'set-mark-command))))
(define-key vi-mode-map (kbd "S") (lambda() (interactive) (beginning-of-line) (call-interactively 'set-mark-command) (forward-line)))
(define-key vi-mode-map (kbd "<backtab>") 'indent-rigidly-left-to-tab-stop)
(define-key vi-mode-map (kbd "<tab>") 'indent-rigidly-right-to-tab-stop)
(define-key vi-mode-map (kbd "<escape>") 'keyboard-quit)
(define-key vi-mode-map (kbd "/") 'isearch-forward)
(define-key vi-mode-map (kbd "?") 'rgrep)
(define-key vi-mode-map (kbd "p") 'yank)
(define-key vi-mode-map (kbd ";") (lambda() (interactive) (end-of-line) (insert-char #x3B)))
(define-key vi-mode-map (kbd "y") 'kill-ring-save)
(define-key vi-mode-map (kbd "g") 'beginning-of-buffer)
(define-key vi-mode-map (kbd "G") 'end-of-buffer)
(define-key vi-mode-map (kbd "z") 'undo)
(define-key vi-mode-map (kbd ":") 'eval-expression)
(define-key vi-mode-map (kbd "x") 'execute-extended-command)
(define-key vi-mode-map (kbd "(") 'previous-buffer)
(define-key vi-mode-map (kbd ")") 'next-buffer)
(define-key vi-mode-map (kbd "b") 'ido-switch-buffer)
(define-key vi-mode-map (kbd "f") 'find-file)
(define-key vi-mode-map (kbd "F") 'quick-find-file)
(define-key vi-mode-map (kbd "SPC") 'vi-off)

(define-key vi-mode-map (kbd "`") 'eshell)
(define-key vi-mode-map (kbd "\\ v p") 'variable-pitch-mode)
(define-key vi-mode-map (kbd "\\ e a") 'edit-abbrevs)
(define-key vi-mode-map (kbd "\\ i t") 'toggle-indent-tabs)
(define-key vi-mode-map (kbd "\\ m m") 'markdown-mode)
(define-key vi-mode-map (kbd "\\ f r") 'french)
(define-key vi-mode-map (kbd "\\ c m") 'cmark)
(define-key vi-mode-map (kbd "\\ s t") 'spaces-to-tabs)
(define-key vi-mode-map (kbd "\\ e r") 'eval-region)

(define-minor-mode vi-mode
	"Ghetto vi mode"
	:lighter " vi"
	:keymap 'vi-mode-map)

(define-key global-map (kbd "C-s") 'save-buffer)
(define-key global-map (kbd "C-n") 'vi-on)
(define-key global-map (kbd "C-w") 'backward-kill-word)
(define-key global-map (kbd "C-k") 'kill-this-buffer)
(define-key global-map (kbd "C-z") 'undo)
(define-key global-map (kbd "C-0") 'delete-other-windows)
(define-key global-map (kbd "C-a") 'other-window)
(define-key global-map (kbd "C-,") 'previous-buffer)
(define-key global-map (kbd "C-.") 'next-buffer)
(define-key global-map (kbd "C-c") nil)
(define-key global-map (kbd "<escape>") 'vi-on)
(define-key global-map (kbd "C-<tab>") 'ido-switch-buffer)
(define-key global-map (kbd "<C-return>") 'line-below)
(define-key global-map (kbd "<f2>") 'dired-here)
(define-key global-map (kbd "C-g") 'goto-line)

(define-key prog-mode-map (kbd "<tab>") 'expand-or-tab)
(define-key prog-mode-map (kbd "<backtab>") 'indent-rigidly-left-to-tab-stop)
(define-key prog-mode-map (kbd "<backspace>") 'backspace-or-unindent)
(define-key prog-mode-map (kbd "<return>") 'newline-and-indent-relative)
(define-key prog-mode-map (kbd "C-SPC") 'unexpand-abbrev)

(define-key text-mode-map (kbd "<tab>") 'expand-or-tab)
(define-key text-mode-map (kbd "<backtab>") 'indent-rigidly-left-to-tab-stop)
(define-key text-mode-map (kbd "<backspace>") 'backspace-or-unindent)
(define-key text-mode-map (kbd "C-SPC") 'unexpand-abbrev)
(define-key text-mode-map (kbd "SPC") 'space-comma-dot)
(define-key text-mode-map (kbd "S-SPC") (lambda() (interactive) (capitalize-word -1)))

(define-key minibuffer-local-map (kbd "<escape>") 'abort-recursive-edit)

(define-key isearch-mode-map (kbd "<return>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<S-return>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<C-return>") 'isearch-exit)
(define-key isearch-mode-map (kbd "<escape>") 'isearch-exit)
(define-key isearch-mode-map (kbd "C-g") 'isearch-exit)

(define-key edit-abbrevs-mode-map (kbd "C-s") 'abbrev-edit-save-buffer)

(add-hook 'prog-mode-hook (lambda() (abbrev-mode 1) (vi-on)))
(add-hook 'minibuffer-setup-hook 'vi-off)
(add-hook 'text-mode-hook (lambda() (abbrev-mode 1) (variable-pitch-mode) (vi-on)))
(add-hook 'eshell-mode-hook 'vi-off)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(with-eval-after-load 'markdown-mode
	(setq markdown-mode-map (make-sparse-keymap))
	(define-key markdown-mode-map (kbd "<return>") 'double-newline))

(with-eval-after-load 'php-mode
	(setq php-mode-map (make-sparse-keymap))
	(define-key php-mode-map (kbd "C-c") 'nil))

(with-eval-after-load 'js-mode
	(define-key js-mode-map (kbd "C-c") 'nil))

(with-eval-after-load 'dired
	(define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)
	(define-key dired-mode-map (kbd "a") 'dired-find-alternate-file)
	(define-key dired-mode-map (kbd "e") 'dired-next-line)
	(define-key dired-mode-map (kbd "o") 'dired-previous-line)
	(define-key dired-mode-map (kbd "i") 'dired-up-directory)
	(define-key dired-mode-map (kbd "x") 'execute-extended-command)
	(define-key dired-mode-map (kbd "b") 'ido-switch-buffer)
	(define-key dired-mode-map (kbd "f") 'find-file)
	(define-key dired-mode-map (kbd "f") 'quick-find-file)
	(define-key dired-mode-map (kbd "/") 'isearch-forward)
	(define-key dired-mode-map (kbd "C-o") nil))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (markdown-mode php-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#fff3e0" :foreground "#303030" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "1ASC" :family "Liberation Mono"))))
 '(eshell-ls-directory ((t (:foreground "#44d" :weight bold))))
 '(eshell-ls-executable ((t (:foreground "#484" :weight bold))))
 '(eshell-prompt ((t (:foreground "#d44" :weight bold))))
 '(font-lock-builtin-face ((t (:underline (:color foreground-color)))))
 '(font-lock-constant-face ((t nil)))
 '(font-lock-function-name-face ((t (:foreground "#048"))))
 '(font-lock-keyword-face ((t (:weight semi-bold))))
 '(font-lock-string-face ((t (:foreground "#084"))))
 '(font-lock-type-face ((t nil)))
 '(font-lock-variable-name-face ((t nil)))
 '(fringe ((t nil)))
 '(isearch ((t (:background "#048" :foreground "#fff"))))
 '(lazy-highlight ((t (:inherit highlight))))
 '(minibuffer-prompt ((t (:foreground "#800"))))
 '(mmm-default-submode-face ((t nil)))
 '(mode-line ((t (:background "#aa8"))))
 '(mode-line-inactive ((t nil)))
 '(php-$this ((t (:slant oblique))))
 '(php-function-call ((t (:inherit font-lock-function-name-face))))
 '(show-paren-match ((t (:inherit highlight))))
 '(variable-pitch ((t (:height 240 :family "Sans Serif")))))
