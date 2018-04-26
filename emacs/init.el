(package-initialize)
(setq package-archives
	'(
		 ("gnu" . "https://elpa.gnu.org/packages/")
		 ("melpa" . "https://melpa.org/packages/")))

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
 '(default ((t (:background "#140601" :foreground "white" :family "Monospace" :height 100))))
 '(fringe ((t (:background "#3E2723"))))
 '(markdown-hr-face ((t (:inherit markdown-markup-face :height 2.0))))
 '(markdown-italic-face ((t (:foreground "#009688" :slant italic))))
 '(markdown-markup-face ((t (:foreground "#FFC107"))))
 '(mode-line ((t (:background "#33691E" :foreground "white"))))
 '(mode-line-inactive ((t (:background "#212121" :foreground "#E0E0E0"))))
 '(variable-pitch ((t (:height 110 :family "sans")))))

;variables
(setq-default inhibit-startup-screen t)
(setq-default make-backup-files nil)
(setq-default major-mode 'text-mode)
(setq-default cursor-type 'bar)
(setq-default fringes-outside-margins t)
(setq-default indent-tabs-mode t)
(setq-default lisp-indent-offset 0)
(setq-default delete-by-moving-to-trash t)
(setq-default vc-follow-symlinks t)
(setq-default backward-delete-char-untabify-method nil)
(put 'dired-find-alternate-file 'disabled nil)

;indentation
(setq-default tab-width 8)
(setq-default c-basic-offset tab-width)
(setq-default css-indent-offset tab-width)
(setq-default python-indent-offset tab-width)
(setq-default sgml-basic-offset tab-width)
(setq-default sh-basic-offset tab-width)
(setq-default js-indent-level tab-width)
(setq-default smie-indent-basic tab-width)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (bbcode-mode markdown-mode wordnut))))

;keys
(defvar my-keys-minor-mode-map (make-sparse-keymap)
	"Keymap for my-keys-minor-mode")

(define-minor-mode my-keys-minor-mode
	"Override major mode keys"
	:init-value t
	:lighter " my keys"
	:keymap my-keys-minor-mode-map)

(my-keys-minor-mode 1)

;kind of vi keybindings
(define-key my-keys-minor-mode-map (kbd "M-j") 'next-line)
(define-key my-keys-minor-mode-map (kbd "M-k") 'previous-line)
(define-key my-keys-minor-mode-map (kbd "M-h") 'backward-char)
(define-key my-keys-minor-mode-map (kbd "M-l") 'forward-char)
(define-key my-keys-minor-mode-map (kbd "M-H") 'beginning-of-line-text)
(define-key my-keys-minor-mode-map (kbd "M-L") 'end-of-line)
(define-key my-keys-minor-mode-map (kbd "M-J") 'forward-paragraph)
(define-key my-keys-minor-mode-map (kbd "M-K") 'backward-paragraph)
(define-key my-keys-minor-mode-map (kbd "C-M-h") 'backward-word)
(define-key my-keys-minor-mode-map (kbd "C-M-l") 'forward-word)
(define-key my-keys-minor-mode-map (kbd "C-M-j") 'scroll-up-command)
(define-key my-keys-minor-mode-map (kbd "C-M-k") 'scroll-down-command)

;window/buffer management
(define-key my-keys-minor-mode-map (kbd "M-1") 'delete-other-windows)
(define-key my-keys-minor-mode-map (kbd "M-2") 'split-window-below)
(define-key my-keys-minor-mode-map (kbd "M-3") 'split-window-right)
(define-key my-keys-minor-mode-map (kbd "M-0") 'delete-window)
(define-key my-keys-minor-mode-map (kbd "M-o") 'other-window)
(define-key my-keys-minor-mode-map (kbd "M-n") 'make-frame-command)
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
(define-key my-keys-minor-mode-map (kbd "C-M-<backspace>") 'kill-whole-line)

; leader
(define-key my-keys-minor-mode-map (kbd "C-z") nil)
(define-key my-keys-minor-mode-map (kbd "C-z w c") 'count-words)
(define-key my-keys-minor-mode-map (kbd "C-z r n") 'rename-buffer)
(define-key my-keys-minor-mode-map (kbd "C-z q") 'save-buffers-kill-terminal)
(define-key my-keys-minor-mode-map (kbd "C-z k b") 'kill-buffer)
(define-key my-keys-minor-mode-map (kbd "C-z l b") 'ibuffer)
(define-key my-keys-minor-mode-map (kbd "C-z f i") 'find-name-dired)
(define-key my-keys-minor-mode-map (kbd "C-z <RET>") 'espeak-line)
(define-key my-keys-minor-mode-map (kbd "C-z m a") 'woman)
(define-key my-keys-minor-mode-map (kbd "C-z x t") 'xterm-here)
(define-key my-keys-minor-mode-map (kbd "C-z t e") (lambda() (interactive) (term "/bin/bash")))
(define-key my-keys-minor-mode-map (kbd "C-z c d") 'cd)
(define-key my-keys-minor-mode-map (kbd "C-z g r") 'rgrep)
(define-key my-keys-minor-mode-map (kbd "C-z e b") 'eval-buffer)
(define-key my-keys-minor-mode-map (kbd "C-z v p") 'variable-pitch-mode)
(define-key my-keys-minor-mode-map (kbd "C-z f +") (lambda() (interactive) (text-scale-adjust 1)))
(define-key my-keys-minor-mode-map (kbd "C-z f -") (lambda() (interactive) (text-scale-adjust -1)))
(define-key my-keys-minor-mode-map (kbd "C-z f 0") (lambda() (interactive) (text-scale-adjust 0)))
(define-key my-keys-minor-mode-map (kbd "C-z w m") 'centre-window-margins)
(define-key my-keys-minor-mode-map (kbd "C-z r u n") 'async-shell-command)
(define-key my-keys-minor-mode-map (kbd "C-z p i p e") 'shell-command-on-region)
(define-key my-keys-minor-mode-map (kbd "C-z s h") 'eshell)
(define-key my-keys-minor-mode-map (kbd "C-z d i r") 'dired)
(define-key my-keys-minor-mode-map (kbd "C-z t m p") (lambda() (interactive) (find-file "~/scratchpad")))
(define-key my-keys-minor-mode-map (kbd "C-z w n") 'wordnut-search)
(define-key my-keys-minor-mode-map (kbd "C-z g i t") 'git-status)

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
	(define-key dired-mode-map (kbd "C-z i") 'dired-sxiv-marked)
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

;git
(with-eval-after-load "git"
	(define-key git-status-mode-map (kbd "j") 'git-next-file)
	(define-key git-status-mode-map (kbd "k") 'git-prev-file))

(defun dired-xdg-open-file ()
	"In dired, open the file with xdg-open"
	(interactive)
	(let* ((file (dired-get-filename)))
		(call-process "xdg-open" nil 0 nil file)))

;hooks
(add-hook 'emacs-lisp-mode-hook 'sensible-defaults)
(add-hook 'js-mode-hook 'sensible-defaults)
(add-hook 'c-mode-hook 'sensible-defaults)
(add-hook 'html-mode-hook 'sensible-defaults)
(add-hook 'css-mode-hook 'sensible-defaults)
(add-hook 'shell-script-mode-hook 'sensible-defaults)

;functions
(defun dired-sxiv-marked ()
	"In dired, open all marked files with xdg-open"
	(interactive)
	(apply 'call-process "sxiv" nil 0 nil (dired-get-marked-files)))

(defun espeak-line ()
	"espeak line"
	(interactive)
	(call-process "espeak-ng" nil 0 nil (thing-at-point 'line t)))

(defun xterm-here()
	(interactive)
	(call-process "stterm" nil 0 nil "-f" "Monospace:pixelsize=14"))

(defun centre-window-margins()
	(interactive)
	(defconst x (/ (- (frame-width) 80) 2))
	(set-window-margins nil x x))

(defun sensible-defaults()
	(setq-default indent-line-function 'indent-relative))
