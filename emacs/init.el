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
(setq-default vc-follow-symlinks t)

(custom-set-variables)
(custom-set-faces
	'(italic ((t (:foreground "lime green" :slant italic)))))

(defvar my-keys-minor-mode-map (make-sparse-keymap)
	"Keymap for my-keys-minor-mode")

(define-minor-mode my-keys-minor-mode
	"Override major mode keys"
	:init-value t
	:lighter " my keys"
	:keymap my-keys-minor-mode-map)

(my-keys-minor-mode 1)

(define-key my-keys-minor-mode-map (kbd "M-1") 'delete-other-windows)
(define-key my-keys-minor-mode-map (kbd "M-2") 'split-window-below)
(define-key my-keys-minor-mode-map (kbd "M-3") 'split-window-right)
(define-key my-keys-minor-mode-map (kbd "M-0") 'delete-window)
(define-key my-keys-minor-mode-map (kbd "M-o") 'other-window)
(define-key my-keys-minor-mode-map (kbd "M-n") 'make-frame-command)
(define-key my-keys-minor-mode-map (kbd "M-j") 'next-line)
(define-key my-keys-minor-mode-map (kbd "M-k") 'previous-line)
(define-key my-keys-minor-mode-map (kbd "M-h") 'backward-char)
(define-key my-keys-minor-mode-map (kbd "M-l") 'forward-char)
(define-key my-keys-minor-mode-map (kbd "S-M-h") 'beginning-of-line)
(define-key my-keys-minor-mode-map (kbd "S-M-l") 'end-of-line)
(define-key my-keys-minor-mode-map (kbd "S-M-j") 'forward-paragraph)
(define-key my-keys-minor-mode-map (kbd "S-M-k") 'backward-paragraph)
(define-key my-keys-minor-mode-map (kbd "C-M-h") 'backward-word)
(define-key my-keys-minor-mode-map (kbd "C-M-l") 'forward-word)
(define-key my-keys-minor-mode-map (kbd "C-M-j") 'scroll-up-command)
(define-key my-keys-minor-mode-map (kbd "C-M-k") 'scroll-down-command)
(define-key my-keys-minor-mode-map (kbd "M-[") 'previous-buffer)
(define-key my-keys-minor-mode-map (kbd "M-]") 'next-buffer)
(define-key my-keys-minor-mode-map (kbd "<escape>") 'keyboard-quit)
(define-key my-keys-minor-mode-map (kbd "M-D") 'backward-kill-word)
(define-key my-keys-minor-mode-map (kbd "<C-tab>") 'indent-region)
(define-key my-keys-minor-mode-map (kbd "M-e") 'execute-extended-command)

; leader
(define-key my-keys-minor-mode-map (kbd "C-z") nil)
(define-key my-keys-minor-mode-map (kbd "C-z w c") 'count-words)
(define-key my-keys-minor-mode-map (kbd "C-z r n") 'rename-buffer)
(define-key my-keys-minor-mode-map (kbd "C-z w m") 'toggle-writing)
(define-key my-keys-minor-mode-map (kbd "C-z q") 'save-buffers-kill-terminal)
(define-key my-keys-minor-mode-map (kbd "C-z k b") 'kill-buffer)
(define-key my-keys-minor-mode-map (kbd "C-z l b") 'list-buffers)
(define-key my-keys-minor-mode-map (kbd "C-z f i") 'find-name-dired)
(define-key my-keys-minor-mode-map (kbd "C-z w t") 'firefox-open-thesaurus)
(define-key my-keys-minor-mode-map (kbd "C-z w d") 'firefox-open-dictionary)
(define-key my-keys-minor-mode-map (kbd "C-z <RET>") 'espeak-line)
(define-key my-keys-minor-mode-map (kbd "C-z m a") 'woman)
(define-key my-keys-minor-mode-map (kbd "C-z x t") 'xterm-here)
(define-key my-keys-minor-mode-map (kbd "C-z t e") (lambda()
	(interactive)
	(term "/bin/bash")))
(define-key my-keys-minor-mode-map (kbd "C-z c d") 'cd)
(define-key my-keys-minor-mode-map (kbd "C-z g r") 'rgrep)
(define-key my-keys-minor-mode-map (kbd "C-z e b") 'eval-buffer)

; typical keys
(define-key my-keys-minor-mode-map (kbd "C-v") 'yank)
(define-key my-keys-minor-mode-map (kbd "C-a") 'mark-whole-buffer)
(define-key my-keys-minor-mode-map (kbd "C-x") 'kill-region)
(define-key my-keys-minor-mode-map (kbd "S-C-x") 'kill-ring-save)
(define-key my-keys-minor-mode-map (kbd "C-s") 'save-buffer)
(define-key my-keys-minor-mode-map (kbd "C-f") 'isearch-forward)
(define-key my-keys-minor-mode-map (kbd "S-C-f") 'isearch-backward)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)
(define-key my-keys-minor-mode-map (kbd "C-o") 'find-file)

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
	(define-key dired-mode-map (kbd "C-z i") 'dired-sxiv-marked))

(defvar writing-p nil)
(defvar cached-mode-line nil)
(defun toggle-writing()
	"writing mode"
	(interactive)
	(if writing-p
		(progn
			(setq mode-line-format cached-mode-line cached-mode-line nil)
			(set-frame-font monospace-font)
			(setq writing-p nil))
		(progn
			(set-frame-font sans-serif-font)
			(setq cached-mode-line mode-line-format mode-line-format nil)
			(sleep-for 0.5)
			(set-frame-width (selected-frame) 80)
			(setq writing-p t))))

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

(defun xterm-here()
	(interactive)
	(call-process "xterm" nil 0 nil "-e" (concat "cd " default-directory " && bash")))

(put 'dired-find-alternate-file 'disabled nil)
