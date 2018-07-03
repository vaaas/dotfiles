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
(setq snippets '(
	("initial-viewport" . "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">")))

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
 '(italic ((t (:slant italic))))
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

;kind of vi keybindings
(global-set-key (kbd "M-j") 'next-line)
(global-set-key (kbd "M-k") 'previous-line)
(global-set-key (kbd "M-h") 'backward-char)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-H") 'beginning-of-line-text)
(global-set-key (kbd "M-L") 'end-of-line)
(global-set-key (kbd "M-J") (lambda() (interactive) (forward-paragraph) (next-line)))
(global-set-key (kbd "M-K") (lambda() (interactive) (backward-paragraph) (backward-paragraph) (next-line)))
(global-set-key (kbd "C-M-h") 'backward-word)
(global-set-key (kbd "C-M-l") 'forward-word)
(global-set-key (kbd "C-M-j") 'scroll-up-command)
(global-set-key (kbd "C-M-k") 'scroll-down-command)
(global-set-key (kbd "C-'") 'quoted-insert)

;window/buffer management
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'delete-window)
(global-set-key (kbd "C-M-o") 'delete-other-windows)
(global-set-key (kbd "C-n") 'make-frame-command)
(global-set-key (kbd "M-+") 'enlarge-window-horizontally)
(global-set-key (kbd "M-_") 'shrink-window-horizontally)
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

;etc
(global-set-key (kbd "<escape>") 'keyboard-quit)
(global-set-key (kbd "M-D") 'backward-kill-word)
(global-set-key (kbd "<C-tab>") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "<backtab>") 'indent-rigidly-left-to-tab-stop)
(global-set-key (kbd "M-e") 'execute-extended-command)
(global-set-key (kbd "M-E") 'eval-expression)
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-w") 'universal-argument)
(global-set-key (kbd "C-q") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-S-q") 'kmacro-end-or-call-macro)
(global-set-key (kbd "<M-tab>") 'ibuffer)


; leader
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-z w c") 'count-words)
(global-set-key (kbd "C-z r n") 'rename-buffer)
(global-set-key (kbd "C-z q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-z k b") 'kill-buffer)
(global-set-key (kbd "C-z f i") (lambda ()
	(interactive)
	(find-name-dired "."
		(concat "*" (read-from-minibuffer "File name: ") "*"))))
(global-set-key (kbd "C-z m a") 'woman)
(global-set-key (kbd "C-z x t") (lambda() (interactive) (call-process "st" nil 0 nil)))
(global-set-key (kbd "C-z t e") (lambda() (interactive) (term "/bin/bash")))
(global-set-key (kbd "C-z c d") 'cd)
(global-set-key (kbd "C-z g r") 'rgrep)
(global-set-key (kbd "C-z e b") 'eval-buffer)
(global-set-key (kbd "C-z v p") 'variable-pitch-mode)
(global-set-key (kbd "C-z f +") (lambda() (interactive) (text-scale-set 2)))
(global-set-key (kbd "C-z f =") (lambda() (interactive) (text-scale-set 0)))
(global-set-key (kbd "C-z f s") (lambda(arg) (interactive "P") (text-scale-set arg)))
(global-set-key (kbd "C-z w m") (lambda()
	(interactive)
	(defconst x (/ (- (frame-width) 80) 2))
	(set-window-margins nil x x)))
(global-set-key (kbd "C-z r u n") 'async-shell-command)
(global-set-key (kbd "C-z |") 'shell-command-on-region)
(global-set-key (kbd "C-z e s") 'eshell)
(global-set-key (kbd "C-z d i r") 'dired)
(global-set-key (kbd "C-z t m p") (lambda() (interactive) (find-file "~/scratchpad")))
(global-set-key (kbd "C-z w n") 'wordnut-search)
(global-set-key (kbd "C-z g i t") 'git-status)
(global-set-key (kbd "C-z s h") 'split-window-below)
(global-set-key (kbd "C-z s v") 'split-window-right)
(global-set-key (kbd "C-z r e f") (lambda() (interactive) (async-shell-command "sxiv -b -- ref/*")))
(global-set-key (kbd "C-z c m") (lambda() (interactive) (shell-command-on-buffer "cmark --smart" t)))
(global-set-key (kbd "C-z y m d") (lambda() (interactive) (insert (format-time-string "%Y-%m-%d"))))
(global-set-key (kbd "C-z s e c") (lambda() (interactive) (insert (format-time-string "%s"))))
(global-set-key (kbd "C-z i n s") (lambda() (interactive) (insert-file-contents (read-file-name "File: "))))
(global-set-key (kbd "C-z m m") (lambda() (interactive) (markdown-mode)))
(global-set-key (kbd "C-z s n") (lambda() (interactive) (insert (cdr (assoc (completing-read "snippet: " snippets) snippets)))))

; typical keys
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-x") 'kill-region)
(global-set-key (kbd "S-C-x") 'kill-ring-save)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-S-r") 'query-replace-regexp)
(global-set-key (kbd "S-C-f") 'isearch-backward)
(global-set-key (kbd "C-o") 'find-file)
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
	(define-key markdown-mode-map (kbd "C-<return>") (lambda()
		(interactive)
		(call-process "espeak-ng" nil 0 nil "-s" "250" (thing-at-point 'line t))))
	(define-key markdown-mode-map (kbd "C-z C-l") 'markdown-insert-link)
	(define-key markdown-mode-map (kbd "C-z S-C-i") 'markdown-insert-image)
	(define-key markdown-mode-map (kbd "C-z C-i") 'markdown-insert-italic)
	(define-key markdown-mode-map (kbd "C-z C-b") 'markdown-insert-bold))

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

(with-eval-after-load "sgml-mode"
	(define-key html-mode-map (kbd "C-z C-b") (lambda(start end) (interactive "r") (wrap-or-insert "<strong>" "</strong>" start end)))
	(define-key html-mode-map (kbd "C-z C-i") (lambda(start end) (interactive "r") (wrap-or-insert "<em>" "</em>" start end)))
	(define-key html-mode-map (kbd "C-z C-a") (lambda(start end) (interactive "r") (wrap-or-insert "<article>" "</article>" start end)))
	(define-key html-mode-map (kbd "C-z C-1") (lambda(start end) (interactive "r") (wrap-or-insert "<h1>" "</h1>" start end)))
	(define-key html-mode-map (kbd "C-z C-2") (lambda(start end) (interactive "r") (wrap-or-insert "<h2>" "</h2>" start end)))
	(define-key html-mode-map (kbd "C-z C-3") (lambda(start end) (interactive "r") (wrap-or-insert "<h3>" "</h3>" start end)))
	(define-key html-mode-map (kbd "C-z C-4") (lambda(start end) (interactive "r") (wrap-or-insert "<h4>" "</h4>" start end)))
	(define-key html-mode-map (kbd "C-z C-5") (lambda(start end) (interactive "r") (wrap-or-insert "<h5>" "</h5>" start end)))
	(define-key html-mode-map (kbd "C-z C-6") (lambda(start end) (interactive "r") (wrap-or-insert "<h6>" "</h6>" start end)))
	(define-key html-mode-map (kbd "C-z C-l") (lambda(start end) (interactive "r") (wrap-or-insert (concat "<a href=\"" (read-from-minibuffer "href: ") "\">") "</a>" start end)))
	(define-key html-mode-map (kbd "C-<return>") (lambda() (interactive) (end-of-line) (newline-and-indent) (wrap-or-insert "<p>" "</p>")))
	(define-key html-mode-map (kbd "C-z C-;") 'comment-region)
	(define-key html-mode-map (kbd "C-z S-C-i") (lambda() (interactive) (insert (concat "<img src=\"" (read-from-minibuffer "src: ") "\"/>")))))

;hooks
(add-hook 'emacs-lisp-mode-hook 'sensible-defaults)
(add-hook 'js-mode-hook 'sensible-defaults)
(add-hook 'c-mode-hook 'sensible-defaults)
(add-hook 'html-mode-hook 'sensible-defaults)
(add-hook 'css-mode-hook 'sensible-defaults)
(add-hook 'shell-script-mode-hook 'sensible-defaults)
(add-hook 'prog-mode-hook (lambda ()
	(auto-complete-mode)))
(add-hook 'eshell-mode-hook (lambda()
	(define-key eshell-mode-map (kbd "M-k") 'eshell-previous-matching-input-from-input)
	(define-key eshell-mode-map (kbd "M-j") 'eshell-next-matching-input-from-input)
	(define-key eshell-mode-map (kbd "M-K") 'eshell-previous-prompt)
	(define-key eshell-mode-map (kbd "M-J") 'eshell-next-prompt)
	(setenv "PAGER" "cat")
	(setenv "TERM" "eshell")))

;functions
(defun sensible-defaults()
	(setq-default indent-line-function 'indent-relative))

(defun dired-xdg-open-file()
	"In dired, open the file with xdg-open"
	(interactive)
	(let* ((file (dired-get-filename)))
		(call-process "xdg-open" nil 0 nil file)))

(defun shell-command-on-buffer(command replace)
	(shell-command-on-region 1 (point-max) command "*shell-output*" replace))

(defun wrap-or-insert (s1 s2 &optional start end)
	(cond ((and start end) (setq
		a start
		b end))
	(t (setq a (point) b (point))))
	(goto-char b)
	(insert s2)
	(goto-char a)
	(insert s1)
	(goto-char (+ a (length s1))))
