; function definitions
(defun capitalize-last-word() (interactive)
	(capitalize-word -1))

(defun backward-kill-line() (interactive)
	(kill-line 0))

(defun recenter-top() (interactive) (recenter 0))

(defun kill-active-region(beg end) (interactive "r")
	(when (region-active-p)
		(kill-region beg end)))

(defun insert-tab-or-indent(beg end) (interactive "r")
	(if (region-active-p)
		(indent-rigidly-right-to-tab-stop beg end)
		(insert-tab)))

(defun spawn(program &rest args)
	(apply 'start-process
		(append (list program program program) args)))

(defun insert-tab() (interactive) (insert-char 9))

(defun insert-space() (interactive) (insert-char 32))

(defun newline-and-indent-relative() (interactive)
	(newline)
	(indent-relative-first-indent-point))

(defun sxiv-ref() (interactive)
	(spawn "sxiv" "-b" "ref/"))

(defun split-newlines(string)
	(split-string string "\n"))

(defun filedb-open() (interactive)
	(find-file
	(ido-completing-read "select a file > "
	(split-newlines
	(shell-command-to-string "xzcat ~/filedb.xz")))))

(defun newline-above() (interactive)
	(beginning-of-line)
	(newline)
	(previous-line))

(defun newline-below() (interactive)
	(end-of-line)
	(newline))

(defun double-newline() (interactive)
	(newline) (newline))

(defun vi-open-above() (interactive)
	(newline-above)
	(vi-mode-off))

(defun vi-open-below() (interactive)
	(newline-below)
	(vi-mode-off))

(defun vi-change-line() (interactive)
	(kill-whole-line)
	(vi-mode-off))

(defun vi-change-next-word() (interactive)
	(kill-word 1)
	(vi-mode-off))

(defun vi-change-previous-word() (interactive)
	(backward-kill-word 1)
	(vi-mode-off))

(defun yank-this-line() (interactive)
	(kill-ring-save (line-beginning-position) (line-end-position)))

(defun paste-below() (interactive)
	(newline-below)
	(yank))

(defun paste-above() (interactive)
	(newline-above)
	(yank))

(defun vi-change-to() (interactive)
	(call-interactively 'zap-to-char)
	(vi-mode-off))

(defun vi-mode-on() (interactive)
	(setq-local cursor-type 'box)
	(vi-mode 1))

(defun vi-mode-off() (interactive)
	(kill-local-variable 'cursor-type)
	(vi-mode -1))

(defun okay-line() (interactive)
	(beginning-of-line-text)
	(insert "✔ ")
	(next-line))

(defun bad-line() (interactive)
	(beginning-of-line-text)
	(insert "✘ ")
	(next-line))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (company markdown-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "#d33682"))))
 '(error ((t (:foreground "#dc322f" :weight bold))))
 '(font-lock-builtin-face ((t (:inherit font-lock-constant-face))))
 '(font-lock-comment-face ((t (:foreground "#839496"))))
 '(font-lock-constant-face ((t (:foreground "#586e75" :weight ultra-bold))))
 '(font-lock-function-name-face ((t (:slant italic))))
 '(font-lock-keyword-face ((t (:weight bold))))
 '(font-lock-string-face ((t (:foreground "#b58900"))))
 '(font-lock-type-face ((t (:foreground "#586e75" :underline t))))
 '(font-lock-variable-name-face ((t (:inherit font-lock-function-name-face))))
 '(highlight ((t (:inherit lazy-highlight))))
 '(ido-first-match ((t (:foreground "#268bd2" :weight bold))))
 '(ido-only-match ((t (:inherit ido-first-match))))
 '(isearch ((t (:background "#268bd2" :foreground "#fdf6e3"))))
 '(isearch-fail ((t (:foreground "#dc322f"))))
 '(italic ((t (:slant italic))))
 '(lazy-highlight ((t (:background "#93a1a1" :foreground "#fdf6e3"))))
 '(link ((t (:underline t))))
 '(markdown-bold-face ((t (:inherit bold :foreground "#586e75"))))
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :height 1.5))))
 '(markdown-italic-face ((t (:inherit italic :foreground "#586e75"))))
 '(markdown-markup-face ((t (:inherit shadow))))
 '(minibuffer-prompt ((t (:foreground "#d33682"))))
 '(region ((t (:inherit isearch))))
 '(shadow ((t (:foreground "#93a1a1")))))

(require 'package)
(add-to-list 'package-archives
	'("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(add-to-list 'write-file-functions 'delete-trailing-whitespace)

; fonts, colors
(setq default-frame-alist '(
	(font . "Monospace 16")
	(background-color . "#fdf6e3")
	(foreground-color . "#073642")))

(add-hook 'window-setup-hook (lambda()
	(scroll-bar-mode -1)
	(tool-bar-mode -1)
	(menu-bar-mode -1)
	(set-fringe-mode 0)))

; keys
(define-key global-map (kbd "C-x") nil)
(define-key global-map (kbd "C-z") nil)

(define-key global-map (kbd "C-s") 'save-buffer)
(define-key global-map (kbd "C-w") 'backward-kill-word)
(define-key global-map (kbd "C-P") 'execute-extended-command)
(define-key global-map (kbd "C-S-P") 'eval-expression)
(define-key global-map (kbd "C-v") 'yank)
(define-key global-map (kbd "C-x") 'kill-active-region)
(define-key global-map (kbd "C-S-X") 'kill-ring-save)
(define-key global-map (kbd "C-e") 'dabbrev-expand)
(define-key global-map (kbd "C-o") 'find-file)
(define-key global-map (kbd "C-S-O") 'filedb-open)
(define-key text-mode-map (kbd "<escape>") 'vi-mode-on)
(define-key prog-mode-map (kbd "<escape>") 'vi-mode-on)
(define-key global-map (kbd "C-f") 'isearch-forward)
(define-key global-map (kbd "C-S-F") 'isearch-backward)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-F") 'isearch-repeat-backward)
(define-key global-map (kbd "C-r") 'replace-string)
(define-key global-map (kbd "C-S-R") 'replace-regexp)
(define-key minibuffer-local-map (kbd "<escape>") 'abort-recursive-edit)
(define-key global-map (kbd "M-o") 'other-window)
(define-key global-map (kbd "M-C-O") 'delete-other-windows)
(define-key global-map (kbd "C-h") 'previous-buffer)
(define-key global-map (kbd "C-l") 'next-buffer)
(define-key global-map (kbd "C-<tab>") 'switch-to-buffer)
(define-key global-map (kbd "M-<tab>") 'mode-line-other-buffer)
(define-key global-map (kbd "M-a") 'set-mark-command)
(define-key global-map (kbd "C-k") 'kill-whole-line)
(define-key global-map (kbd "C-q") 'kill-this-buffer)
(define-key global-map (kbd "C-d") 'kill-word)
(define-key global-map (kbd "C-`") 'eshell)
(define-key global-map (kbd "C-<return>") 'newline-below)
(define-key global-map (kbd "S-<return>") 'newline-above)
(define-key global-map (kbd "M-q") 'kmacro-start-macro)
(define-key global-map (kbd "M-Q") 'kmacro-end-or-call-macro)
(define-key global-map (kbd "M-l") 'right-char)
(define-key global-map (kbd "M-h") 'left-char)
(define-key global-map (kbd "M-j") 'next-line)
(define-key global-map (kbd "M-k") 'previous-line)
(define-key global-map (kbd "C-M-h") 'left-word)
(define-key global-map (kbd "C-M-l") 'right-word)
(define-key global-map (kbd "M-H") 'beginning-of-line-text)
(define-key global-map (kbd "M-L") 'end-of-line)
(define-key global-map (kbd "M-K") 'backward-paragraph)
(define-key global-map (kbd "M-J") 'forward-paragraph)
(define-key global-map (kbd "C-y") 'yank-this-line)
(define-key global-map (kbd "C-]") 'recenter-top)
(define-key global-map (kbd "C-.") 'repeat)
(define-key text-mode-map (kbd "S-SPC") 'capitalize-last-word)

(define-key global-map (kbd "C-c e b") 'eval-buffer)
(define-key global-map (kbd "C-c r e f") 'sxiv-ref)
(define-key global-map (kbd "C-c w c") 'count-words-region)
(define-key global-map (kbd "C-c a") 'mark-whole-buffer)
(define-key global-map (kbd "C-c v p") 'variable-pitch-mode)
(define-key global-map (kbd "C-c f") 'text-scale-adjust)
(define-key global-map (kbd "C-c i b") 'ibuffer)
(define-key global-map (kbd "C-c h") 'help)
(define-key global-map (kbd "C-c m m") 'markdown-mode)
(define-key text-mode-map (kbd "C-c o k") 'okay-line)
(define-key text-mode-map (kbd "C-c n o") 'bad-line)
(define-key global-map (kbd "C-c e a") 'edit-abbrevs)

(define-key prog-mode-map (kbd "<tab>") 'insert-tab-or-indent)
(define-key text-mode-map (kbd "<tab>") 'insert-tab-or-indent)
(define-key emacs-lisp-mode-map (kbd "<tab>") 'insert-tab-or-indent)
(define-key global-map (kbd "<backtab>") 'indent-rigidly-left-to-tab-stop)

(define-key prog-mode-map (kbd "<return>") 'newline-and-indent-relative)
(define-key text-mode-map (kbd "<C-return>") 'newline-and-indent-relative)
(define-key emacs-lisp-mode-map (kbd "<return>") 'newline-and-indent-relative)

(define-key text-mode-map (kbd "<backspace>") 'backward-delete-char)
(define-key prog-mode-map (kbd "<backspace>") 'backward-delete-char)
(define-key emacs-lisp-mode-map (kbd "<backspace>") 'backward-delete-char)

(define-key text-mode-map (kbd "<return>") 'double-newline)
(define-key text-mode-map (kbd "C-SPC") 'unexpand-abbrev)

(with-eval-after-load 'js
	(define-key js-mode-map (kbd "<tab>") 'insert-tab-or-indent))

(with-eval-after-load 'company
	(define-key company-active-map (kbd "M-n") nil)
	(define-key company-active-map (kbd "M-p") nil)
	(define-key company-active-map (kbd "C-j") 'company-select-next)
	(define-key company-active-map (kbd "C-k") 'company-select-previous)
	(define-key company-active-map (kbd "<tab>") 'company-complete-selection)
	(define-key company-active-map (kbd "<escape>") 'company-abort))

(with-eval-after-load 'markdown-mode
	(define-key markdown-mode-map	(kbd "C-x") nil)
	(define-key markdown-mode-map	(kbd "C-b") 'markdown-insert-bold)
	(define-key markdown-mode-map	(kbd "C-i") 'markdown-insert-italic)
	(define-key markdown-mode-map (kbd "C-c l") 'markdown-insert-link)
	(define-key markdown-mode-map (kbd "C-c i") 'markdown-insert-image))

(add-hook 'ido-setup-hook (lambda()
	(define-key ido-completion-map (kbd "C-j") 'ido-next-match)
	(define-key ido-completion-map (kbd "C-k") 'ido-prev-match)))

(with-eval-after-load 'ibuffer
	(define-key ibuffer-mode-map (kbd "j") 'forward-line)
	(define-key ibuffer-mode-map (kbd "k") 'previous-line))

(add-hook 'eshell-mode-hook (lambda()
	(define-key eshell-mode-map (kbd "C-`") 'mode-line-other-buffer)
	(define-key eshell-mode-map (kbd "M-<tab>") nil)))

; variables
(setq-default mode-line-format nil)
(setq-default left-margin-width 1 right-margin-width 1)
(setq-default tab-width 3)
(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(setq frame-resize-pixelwise t)
(setq-default lsp-enable-snippet nil)
(setq company-idle-delay 0.1)
(setq vc-follow-symlinks t)
(setq-default cursor-type 'bar)
(setq-default require-final-newline nil)
(setq save-abbrevs 'silently)
(setq-default abbrev-all-caps t)

; default modes
(setq-default shift-select-mode nil)
(setq-default indent-tabs-mode t)
(ido-mode t)
(electric-indent-mode -1)
(electric-pair-mode 1)
(global-visual-line-mode t)
(line-number-mode -1)
(set-window-margins (selected-window) 1 1)

; hooks
(add-hook 'js-mode-hook (lambda()
	(company-mode)
	(abbrev-mode)))
(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'markdown-mode-hook (lambda()
	(setq require-final-newline nil)
	(variable-pitch-mode)
	(text-scale-increase 2)
	(abbrev-mode)))
(add-hook 'python-mode-hook (lambda()
	(setq indent-tabs-mode t
		tab-width 3
		py-indent-tabs-mode t)))

; vi mode
(setq vi-mode-map (make-sparse-keymap))
(define-key vi-mode-map (kbd "i") 'vi-mode-off)
(define-key vi-mode-map (kbd "h") 'left-char)
(define-key vi-mode-map (kbd "l") 'right-char)
(define-key vi-mode-map (kbd "k") 'previous-line)
(define-key vi-mode-map (kbd "j") 'next-line)
(define-key vi-mode-map (kbd "o") 'newline)
(define-key vi-mode-map (kbd "O") 'newline-above)
(define-key vi-mode-map (kbd "w") 'right-word)
(define-key vi-mode-map (kbd "b") 'left-word)
(define-key vi-mode-map (kbd "}") 'forward-paragraph)
(define-key vi-mode-map (kbd "{") 'backward-paragraph)
(define-key vi-mode-map (kbd "J") 'forward-paragraph)
(define-key vi-mode-map (kbd "K") 'backward-paragraph)
(define-key vi-mode-map (kbd "H") 'beginning-of-line-text)
(define-key vi-mode-map (kbd "L") 'end-of-line)
(define-key vi-mode-map (kbd "G") 'goto-line)
(define-key vi-mode-map (kbd "v") 'set-mark-command)
(define-key vi-mode-map (kbd "D") 'kill-ring)
(define-key vi-mode-map (kbd "Y") 'kill-ring-save)
(define-key vi-mode-map (kbd "d w") 'kill-word)
(define-key vi-mode-map (kbd "d b") 'backward-kill-word)
(define-key vi-mode-map (kbd "d d") 'kill-whole-line)
(define-key vi-mode-map (kbd ".") 'repeat)
(define-key vi-mode-map (kbd "u") 'undo)
(define-key vi-mode-map (kbd "/") 'isearch-forward)
(define-key vi-mode-map (kbd "p") 'paste-below)
(define-key vi-mode-map (kbd "P") 'paste-above)
(define-key vi-mode-map (kbd "SPC") 'execute-extended-command)
(define-key vi-mode-map (kbd "o") 'vi-open-below)
(define-key vi-mode-map (kbd "O") 'vi-open-above)
(define-key vi-mode-map (kbd "q") 'kmacro-start-macro)
(define-key vi-mode-map (kbd "Q") 'kmacro-end-or-call-macro)
(define-key vi-mode-map (kbd "c c") 'vi-change-line)
(define-key vi-mode-map (kbd "c w") 'vi-change-next-word)
(define-key vi-mode-map (kbd "c b") 'vi-change-previous-word)
(define-key vi-mode-map (kbd "y y") 'yank-this-line)
(define-key vi-mode-map (kbd "d t") 'zap-to-char)
(define-key vi-mode-map (kbd "d $") 'kill-line)
(define-key vi-mode-map (kbd "d ^") 'backward-kill-line)
(define-key vi-mode-map (kbd "c t") 'vi-change-to)
(define-key vi-mode-map (kbd "n") 'isearch-repeat-forward)
(define-key vi-mode-map (kbd "N") 'isearch-repeat-backward)
(define-minor-mode vi-mode
	"vi-like key bindings without modifier keys"
	:keymap vi-mode-map)
