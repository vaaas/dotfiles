; -*- lexical-binding: t -*-
; various hooks

(add-hook 'prog-mode-hook (lambda()
	(abbrev-mode 1)
	(vas-normal-mode-on)))

(add-hook 'minibuffer-setup-hook 'vas-normal-mode-off)

(add-hook 'text-mode-hook 'vas-normal-mode-off)

(add-hook 'eshell-mode-hook 'vas-normal-mode-off)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'ido-minibuffer-setup-hook (lambda()
	(define-key ido-completion-map (kbd "C-e") 'ido-next-match)
	(define-key ido-completion-map (kbd "C-o") 'ido-prev-match)))

; js and php mode define their own prettify-symbols that override the defaults, so clear them first
(add-hook 'js-mode-hook (lambda()
	(setq eval-process "node")
	(kill-local-variable 'prettify-symbols-alist)
	(prettify-symbols-mode 1)))

(add-hook 'php-mode-hook (lambda()
	(kill-local-variable 'prettify-symbols-alist)
	(prettify-symbols-mode 1)))

(add-hook 'change-major-mode-hook (lambda ()
	(kill-local-variable 'vas-normal-bonus-mode)))

(with-eval-after-load 'purescript-mode
	(add-hook 'purescript-mode-hook 'prettify-symbols-mode))