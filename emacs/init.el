(setq package-archives
	'(
		 ("gnu" . "https://elpa.gnu.org/packages/")
		 ("melpa" . "https://melpa.org/packages/")))

(defconst monospace-font "Monospace:pixelsize=18")
(defconst sans-serif-font "Roboto Condensed:pixelsize=18")
(add-to-list 'default-frame-alist (cons 'font monospace-font))
(add-to-list 'default-frame-alist '(width . 80))
(add-to-list 'default-frame-alist '(height . 20))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-visual-line-mode 1)
(load "~/.emacs.d/color-theme-molokai.el")
(color-theme-molokai)

(setq-default inhibit-startup-screen t)
(setq-default make-backup-files nil)
(setq-default major-mode 'text-mode)
(setq-default cursor-type 'bar)
(setq-default fringes-outside-margins t)
(setq-default indent-tabs-mode t)
(setq-default tab-width 8)
(setq-default lisp-indent-offset 0)

(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'css-indent-offset 'tab-width)
(defvaralias 'lisp-indent-offset 'tab-width)
(defvaralias 'python-indent-offset 'tab-width)
(defvaralias 'sgml-basic-offset 'tab-width)
(defvaralias 'sh-basic-offset 'tab-width)
(defvaralias 'js-indent-level 'tab-width)

(add-hook 'lisp-mode-hook 'sensible-indentation)
(add-hook 'python-mode-hook 'sensible-indentation)
(add-hook 'javascript-mode-hook 'sensible-indentation)
(add-hook 'css-mode-hook 'sensible-indentation)
(add-hook 'term-mode-hook (lambda ()
	(define-key term-raw-map (kbd "M-]") 'next-buffer)
	(define-key term-raw-map (kbd "<f1>") 'term-line-mode)
	(local-set-key (kbd "<f2>") 'term-char-mode)))
(add-hook 'shell-mode-hook 'shell-hooks-function)
(add-hook 'eshell-mode-hook 'shell-hooks-function)

(global-set-key "\t" 'tab-to-tab-stop)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-n") 'make-frame-command)
(global-set-key (kbd "M-j") 'next-line)
(global-set-key (kbd "M-k") 'previous-line)
(global-set-key (kbd "M-h") 'backward-char)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-H") 'beginning-of-line)
(global-set-key (kbd "M-L") 'end-of-line)
(global-set-key (kbd "M-J") 'forward-paragraph)
(global-set-key (kbd "M-K") 'backward-paragraph)
(global-set-key (kbd "C-M-h") 'backward-word)
(global-set-key (kbd "C-M-l") 'forward-word)
(global-set-key (kbd "C-M-j") 'scroll-up-command)
(global-set-key (kbd "C-M-k") 'scroll-down-command)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "<f12>") 'toggle-frame-width)
(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)
(global-set-key (kbd "<f10>") 'toggle-frame-font)
(global-set-key (kbd "<f9>") 'rename-buffer)
(global-set-key (kbd "<escape>") 'keyboard-quit)
(global-unset-key (kbd "<f1>"))
(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "C-z"))

(defvar frame-width-wide t)
(defvar cached-mode-line nil)
(defun toggle-frame-width()
	"toggle frame width"
	(interactive)
	(if frame-width-wide
		(progn
			(set-window-fringes nil 0 0)
			(defconst x (/ (- (frame-width) 80) 2))
			(set-window-margins nil x x)
			(setq cached-mode-line mode-line-format mode-line-format nil)
			(setq frame-width-wide nil))
		(progn
			(set-window-fringes nil 11 11)
			(set-window-margins nil 0 0)
			(setq mode-line-format cached-mode-line cached-mode-line nil)
			(setq frame-width-wide t))))


(defvar frame-font-proportional nil)
(defun toggle-frame-font()
	"toggle between sans serif and monospace font"
	(interactive)
	(if frame-font-proportional
		(progn
			(set-frame-font monospace-font)
			(setq frame-font-proportional nil))
		(progn
			(set-frame-font sans-serif-font)
			(setq frame-font-proportional t))))

(defun sensible-indentation()
	"sensible indentation"
	(setq indent-tabs-mode t)
	(setq tab-width 8))

(defun shell-hooks-function()
	"unset some bad bindings"
	(local-unset-key (kbd "C-M-l")))

(defun eshell/clear ()
	"Clear the eshell buffer."
	(let ((inhibit-read-only t))
		(erase-buffer)
		(eshell-send-input)))
