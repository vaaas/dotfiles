(package-initialize)

;variables
(setq-default
	package-archives '(
		("gnu" . "https://elpa.gnu.org/packages/")
		("melpa" . "https://melpa.org/packages/"))
	package-selected-packages (quote (
		(auto-complete bbcode-mode markdown-mode wordnut)))
	inhibit-startup-screen t
	make-backup-files nil
	major-mode 'text-mode
	cursor-type 'bar
	fringes-outside-margins t
	indent-tabs-mode t
	lisp-indent-offset 0
	delete-by-moving-to-trash t
	vc-follow-symlinks t
	backward-delete-char-untabify-method nil
	find-name-arg "-iname"
	tab-width 8
	c-basic-offset tab-width
	css-indent-offset tab-width
	python-indent-offset tab-width
	sgml-basic-offset tab-width
	sh-basic-offset tab-width
	js-indent-level tab-width
	smie-indent-basic tab-width)
(put 'dired-find-alternate-file 'disabled nil)

;looks
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(electric-indent-mode -1)
(global-visual-line-mode 1)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#140601" :foreground "white" :family "Monospace" :height 105))))
 '(fringe ((t (:background "#3E2723"))))
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :height 1.25))))
 '(markdown-hr-face ((t (:inherit markdown-markup-face :height 2.0))))
 '(markdown-italic-face ((t (:foreground "#009688" :slant italic))))
 '(markdown-markup-face ((t (:foreground "#FFC107"))))
 '(mode-line ((t (:background "#33691E" :foreground "white"))))
 '(mode-line-inactive ((t (:background "#212121" :foreground "#E0E0E0"))))
 '(variable-pitch ((t (:height 110 :family "sans")))))

;; Add parsing of jshint output in compilation mode
;(add-to-list 'compilation-error-regexp-alist-alist '(jshint "^\\(.*\\): line \\([0-9]+\\), col \\([0-9]+\\), " 1 2 3))
;(add-to-list 'compilation-error-regexp-alist 'jshint)

;keys
(defvar my-keys-minor-mode-map (make-sparse-keymap)
	"Keymap for my-keys-minor-mode")

(define-minor-mode my-keys-minor-mode
	"Override major mode keys"
	:init-value t
	:keymap my-keys-minor-mode-map)

(my-keys-minor-mode 1)

;kind of vi keybindings
(define-key my-keys-minor-mode-map (kbd "M-j") 'next-line)
(define-key my-keys-minor-mode-map (kbd "M-k") 'previous-line)
(define-key my-keys-minor-mode-map (kbd "M-h") 'backward-char)
(define-key my-keys-minor-mode-map (kbd "M-l") 'forward-char)
(define-key my-keys-minor-mode-map (kbd "M-m") 'beginning-of-line-text)
(define-key my-keys-minor-mode-map (kbd "M-n") 'end-of-line)
(define-key my-keys-minor-mode-map (kbd "C-M-n") (lambda() (interactive) (forward-paragraph) (next-line)))
(define-key my-keys-minor-mode-map (kbd "C-M-m") 'backward-paragraph)
(define-key my-keys-minor-mode-map (kbd "C-M-h") 'backward-word)
(define-key my-keys-minor-mode-map (kbd "C-M-l") 'forward-word)
(define-key my-keys-minor-mode-map (kbd "C-M-j") 'scroll-up-command)
(define-key my-keys-minor-mode-map (kbd "C-M-k") 'scroll-down-command)
(define-key my-keys-minor-mode-map (kbd "C-'") 'quoted-insert)

;window/buffer management
(define-key my-keys-minor-mode-map (kbd "M-o") 'other-window)
(define-key my-keys-minor-mode-map (kbd "M-O") 'delete-window)
(define-key my-keys-minor-mode-map (kbd "C-M-o") 'delete-other-windows)
(define-key my-keys-minor-mode-map (kbd "C-n") 'make-frame-command)
(define-key my-keys-minor-mode-map (kbd "M-+") 'enlarge-window-horizontally)
(define-key my-keys-minor-mode-map (kbd "M-_") 'shrink-window-horizontally)
(define-key my-keys-minor-mode-map (kbd "M-[") 'previous-buffer)
(define-key my-keys-minor-mode-map (kbd "M-]") 'next-buffer)

;etc
(define-key my-keys-minor-mode-map (kbd "<escape>") 'keyboard-quit)
(define-key my-keys-minor-mode-map (kbd "M-D") 'backward-kill-word)
(define-key my-keys-minor-mode-map (kbd "<C-tab>") 'indent-rigidly-right-to-tab-stop)
(define-key my-keys-minor-mode-map (kbd "<backtab>") 'indent-rigidly-left-to-tab-stop)
(define-key my-keys-minor-mode-map (kbd "M-e") 'execute-extended-command)
(define-key my-keys-minor-mode-map (kbd "M-E") 'eval-expression)
(define-key my-keys-minor-mode-map (kbd "C-M-<backspace>") 'kill-whole-line)
(define-key my-keys-minor-mode-map (kbd "C-w") 'universal-argument)
(define-key my-keys-minor-mode-map (kbd "C-q") 'kmacro-start-macro-or-insert-counter)
(define-key my-keys-minor-mode-map (kbd "C-S-q") 'kmacro-end-or-call-macro)

; leader
(define-key my-keys-minor-mode-map (kbd "C-z") nil)
(define-key my-keys-minor-mode-map (kbd "C-z w c") 'count-words)
(define-key my-keys-minor-mode-map (kbd "C-z r n") 'rename-buffer)
(define-key my-keys-minor-mode-map (kbd "C-z q") 'save-buffers-kill-terminal)
(define-key my-keys-minor-mode-map (kbd "C-z k b") 'kill-buffer)
(define-key my-keys-minor-mode-map (kbd "C-z l b") 'ibuffer)
(define-key my-keys-minor-mode-map (kbd "C-z f i") (lambda ()
	(interactive)
	(find-name-dired "."
		(concat "*" (read-from-minibuffer "File name: ") "*"))))
(define-key my-keys-minor-mode-map (kbd "C-<return>") (lambda()
	(interactive)
	(call-process "espeak-ng" nil 0 nil "-s" "250" (thing-at-point 'line t))))
(define-key my-keys-minor-mode-map (kbd "C-z m a") 'woman)
(define-key my-keys-minor-mode-map (kbd "C-z x t") (lambda() (interactive) (call-process "st" nil 0 nil)))
(define-key my-keys-minor-mode-map (kbd "C-z t e") (lambda() (interactive) (term "/bin/bash")))
(define-key my-keys-minor-mode-map (kbd "C-z c d") 'cd)
(define-key my-keys-minor-mode-map (kbd "C-z g r") 'rgrep)
(define-key my-keys-minor-mode-map (kbd "C-z e b") 'eval-buffer)
(define-key my-keys-minor-mode-map (kbd "C-z v p") 'variable-pitch-mode)
(define-key my-keys-minor-mode-map (kbd "C-z f +") (lambda() (interactive) (text-scale-set 2)))
(define-key my-keys-minor-mode-map (kbd "C-z f =") (lambda() (interactive) (text-scale-set 0)))
(define-key my-keys-minor-mode-map (kbd "C-z f s") (lambda(arg) (interactive "P") (text-scale-set arg)))
(define-key my-keys-minor-mode-map (kbd "C-z w m") (lambda()
	(interactive)
	(defconst x (/ (- (frame-width) 80) 2))
	(set-window-margins nil x x)))
(define-key my-keys-minor-mode-map (kbd "C-z r u n") 'async-shell-command)
(define-key my-keys-minor-mode-map (kbd "C-z p i p e") 'shell-command-on-region)
(define-key my-keys-minor-mode-map (kbd "C-z s h") 'eshell)
(define-key my-keys-minor-mode-map (kbd "C-z d i r") 'dired)
(define-key my-keys-minor-mode-map (kbd "C-z t m p") (lambda() (interactive) (find-file "~/scratchpad")))
(define-key my-keys-minor-mode-map (kbd "C-z w n") 'wordnut-search)
(define-key my-keys-minor-mode-map (kbd "C-z g i t") 'git-status)
(define-key my-keys-minor-mode-map (kbd "C-z s h") 'split-window-below)
(define-key my-keys-minor-mode-map (kbd "C-z s v") 'split-window-right)
(define-key my-keys-minor-mode-map (kbd "C-z r e f") (lambda() (interactive) (async-shell-command "sxiv -b -- ref/*")))

; typical keys
(define-key my-keys-minor-mode-map (kbd "C-v") 'yank)
(define-key my-keys-minor-mode-map (kbd "C-a") 'mark-whole-buffer)
(define-key my-keys-minor-mode-map (kbd "C-x") 'kill-region)
(define-key my-keys-minor-mode-map (kbd "S-C-x") 'kill-ring-save)
(define-key my-keys-minor-mode-map (kbd "C-s") 'save-buffer)
(define-key my-keys-minor-mode-map (kbd "C-f") 'isearch-forward)
(define-key my-keys-minor-mode-map (kbd "C-r") 'query-replace)
(define-key my-keys-minor-mode-map (kbd "C-S-r") 'query-replace-regexp)
(define-key my-keys-minor-mode-map (kbd "S-C-f") 'isearch-backward)
(define-key my-keys-minor-mode-map (kbd "C-o") 'find-file)
(define-key prog-mode-map (kbd "<return>") 'newline-and-indent)

;search mode
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)

;buffer mode
(define-key Buffer-menu-mode-map (kbd "j") 'next-line)
(define-key Buffer-menu-mode-map (kbd "k") 'previous-line)

;apropos mode
(with-eval-after-load "apropos"
	(define-key apropos-mode-map (kbd "j") 'forward-button)
	(define-key apropos-mode-map (kbd "k") 'backward-button))

;help mode
(with-eval-after-load "help-mode"
	(define-key help-mode-map (kbd "j") 'forward-button)
	(define-key help-mode-map (kbd "k") 'backward-button)
	(define-key help-mode-map (kbd "h") 'help-go-back)
	(define-key help-mode-map (kbd "l") 'help-go-forward)
	(define-key help-mode-map (kbd "?") 'describe-mode))

;grep mode
(with-eval-after-load "grep"
	(define-key grep-mode-map (kbd "j") 'compilation-next-error)
	(define-key grep-mode-map (kbd "k") 'compilation-previous-error)
	(define-key grep-mode-map (kbd "J") 'compilation-next-file)
	(define-key grep-mode-map (kbd "K") 'compilation-previous-file))

;markdown
(with-eval-after-load "markdown-mode"
	(define-key markdown-mode-map (kbd "C-z m a") 'markdown-insert-link)
	(define-key markdown-mode-map (kbd "C-z m i") 'markdown-insert-image)
	(define-key markdown-mode-map (kbd "C-z m e") 'markdown-insert-italic)
	(define-key markdown-mode-map (kbd "C-z m s") 'markdown-insert-bold))

;dired
(with-eval-after-load "dired"
	(define-key dired-mode-map (kbd "<C-return>") 'dired-xdg-open-file)
	(define-key dired-mode-map (kbd "C-z i") (lambda()
		(interactive)
		(apply 'call-process "sxiv" nil 0 nil (dired-get-marked-files))))
	(define-key dired-mode-map (kbd "j") 'dired-next-line)
	(define-key dired-mode-map (kbd "k") 'dired-previous-line)
	(put 'dired-find-alternate-file 'disabled nil)
	(define-key dired-mode-map (kbd "h") (lambda() (interactive) (find-alternate-file "..")))
	(define-key dired-mode-map (kbd "l") 'dired-find-alternate-file)
	(define-key dired-mode-map (kbd "J") 'forward-page)
	(define-key dired-mode-map (kbd "K") 'backward-page)
	(define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file))

;ibuffer
(with-eval-after-load "ibuffer"
	(define-key ibuffer-mode-map (kbd "j") 'ibuffer-forward-line)
	(define-key ibuffer-mode-map (kbd "k") 'ibuffer-backward-line))

(with-eval-after-load "auto-complete"
	(setq ac-sources '(ac-source-words-in-all-buffer)))

;hooks
(add-hook 'emacs-lisp-mode-hook 'sensible-defaults)
(add-hook 'js-mode-hook 'sensible-defaults)
(add-hook 'c-mode-hook 'sensible-defaults)
(add-hook 'html-mode-hook 'sensible-defaults)
(add-hook 'css-mode-hook 'sensible-defaults)
(add-hook 'shell-script-mode-hook 'sensible-defaults)
(add-hook 'prog-mode-hook (lambda ()
	(auto-complete-mode)
	(electric-pair-mode)))

;functions
(defun sensible-defaults()
	(setq-default indent-line-function 'indent-relative))

(defun dired-xdg-open-file ()
	"In dired, open the file with xdg-open"
	(interactive)
	(let* ((file (dired-get-filename)))
		(call-process "xdg-open" nil 0 nil file)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
