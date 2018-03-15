(setq package-archives
	'(
		 ("gnu" . "https://elpa.gnu.org/packages/")
		 ("melpa" . "https://melpa.org/packages/")))

(defconst monospace-font "Monospace:pixelsize=18")
(defconst sans-serif-font "Sans:pixelsize=16")
(add-to-list 'default-frame-alist (cons 'font monospace-font))
(add-to-list 'default-frame-alist '(width . 80))
(add-to-list 'default-frame-alist '(height . 20))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(electric-indent-mode -1)
(global-visual-line-mode 1)

(setq-default inhibit-startup-screen t)
(setq-default make-backup-files nil)
(setq-default major-mode 'text-mode)
(setq-default cursor-type 'bar)
(setq-default fringes-outside-margins t)
(setq-default indent-tabs-mode t)
(setq-default lisp-indent-offset 0)
(setq-default delete-by-moving-to-trash t)

(setq-default tab-width 8)
(setq-default c-basic-offset tab-width)
(setq-default css-indent-offset tab-width)
(setq-default python-indent-offset tab-width)
(setq-default sgml-basic-offset tab-width)
(setq-default sh-basic-offset tab-width)
(setq-default js-indent-level tab-width)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(italic ((t (:foreground "lime green" :slant italic)))))

(add-hook 'term-mode-hook (lambda ()
	(define-key term-raw-map (kbd "M-]") 'next-buffer)
	(define-key term-raw-map (kbd "S-<f1>") 'execute-extended-command)
  	(define-key term-raw-map (kbd "<f1>") 'term-line-mode)
	(define-key term-raw-map (kbd "C-c") 'term-send-raw)
	(local-set-key (kbd "<f2>") 'term-char-mode)))
(add-hook 'shell-mode-hook 'shell-hooks-function)
(add-hook 'eshell-mode-hook 'shell-hooks-function)
(add-hook 'markdown-mode-hook (lambda ()
	(local-unset-key (kbd "C-x"))
	(local-unset-key (kbd "C-c"))
	(local-set-key (kbd "C-z m a") 'markdown-insert-link)
	(local-set-key (kbd "C-z m i") 'markdown-insert-image)
	(local-set-key (kbd "C-z m e") 'markdown-insert-italic)
	(local-set-key (kbd "C-z m s") 'markdown-insert-bold)))
(add-hook 'nxml-mode-hook (lambda ()
	(local-unset-key (kbd "M-h"))))

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
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "<escape>") 'keyboard-quit)
(global-unset-key (kbd "<f1>"))
(global-unset-key (kbd "<f2>"))
(global-set-key (kbd "M-D") 'backward-kill-word)
(global-set-key (kbd "<C-tab>") 'indent-region)
(global-set-key (kbd "<RET>") 'newline-and-indent)
(global-set-key (kbd "<backspace>") 'backward-delete-char)
(global-set-key (kbd "M-e") 'execute-extended-command)
; leader
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z w c") 'count-words)
(global-set-key (kbd "C-z r n") 'rename-buffer)
(global-set-key (kbd "C-z w m") 'toggle-writing)
(global-set-key (kbd "C-z q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-z k b") 'kill-buffer)
(global-set-key (kbd "C-z l b") 'list-buffers)
(global-set-key (kbd "C-z f i") 'find-name-dired)
(global-set-key (kbd "C-z w t") 'firefox-open-thesaurus)
(global-set-key (kbd "C-z w d") 'firefox-open-dictionary)
(global-set-key (kbd "C-z <RET>") 'espeak-line)
; typical keys
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-x") 'kill-region)
(global-set-key (kbd "C-c") 'kill-ring-save)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-S-f") 'isearch-backward)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)
(global-set-key (kbd "C-o") 'find-file)

;dired
(eval-after-load "dired" '(progn
	(define-key dired-mode-map (kbd "<C-return>") 'dired-xdg-open-file)
	(define-key dired-mode-map (kbd "C-z i") 'dired-sxiv-marked)))


(defvar writing-p nil)
(defvar cached-mode-line nil)
(defun toggle-writing()
	"writing mode"
	(interactive)
	(if writing-p
		(progn
			(set-window-fringes nil 11 11)
			(set-window-margins nil 0 0)
			(setq mode-line-format cached-mode-line cached-mode-line nil)
			(set-frame-font monospace-font)
			(toggle-frame-fullscreen)
			(setq writing-p nil))
		(progn
			(set-frame-font sans-serif-font)
			(toggle-frame-fullscreen)
			(sleep-for 0.5)
			(set-window-fringes nil 0 0)
			(defconst x (/ (- (frame-width) 80) 2))
			(set-window-margins nil x x)
			(setq cached-mode-line mode-line-format mode-line-format nil)
			(setq writing-p t))))

(defun shell-hooks-function()
	"unset some bad bindings"
	(local-unset-key (kbd "C-M-l")))

(defun eshell/clear ()
	"Clear the eshell buffer."
	(let ((inhibit-read-only t))
		(erase-buffer)
		(eshell-send-input)))

(defun dired-xdg-open-file ()
	"In dired, open the file with xdg-open"
	(interactive)
	(let* ((file (dired-get-filename)))
		(call-process "xdg-open" nil 0 nil file)))

(defun dired-sxiv-marked ()
	"In dired, open all marked files with xdg-open"
	(interactive)
	(apply 'call-process "sxiv" nil 0 nil (dired-get-marked-files)))

(defun firefox-open-new-window (url)
	"open url in a new firefox window"
	(call-process "firefox" nil 0 nil "--new-window" url))

(defun firefox-open-thesaurus ()
	"open thesaurus.com definition in firefox"
	(interactive)
	(firefox-open-new-window (concat "http://www.thesaurus.com/browse/" (read-string "word: "))))

(defun firefox-open-dictionary ()
	"open dictionary.com definition in firefox"
	(interactive)
	(firefox-open-new-window (concat "http://www.dictionary.com/browse/" (read-string "word: "))))

(defun espeak-line ()
	"espeak line"
	(interactive)
	(call-process "espeak-ng" nil 0 nil (thing-at-point 'line t)))

(defun frame-sans-serif-font()
	(interactive)
	(set-frame-font sans-serif-font))

(defun frame-monospace-font()
	(interactive)
	(set-frame-font monospace-font))

(put 'dired-find-alternate-file 'disabled nil)
