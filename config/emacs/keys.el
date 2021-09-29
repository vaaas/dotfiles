;; -*- lexical-binding: t -*-
(define-key global-map (kbd "<backspace>") 'backspace-or-unindent)
(define-key global-map (kbd "<tab>") 'expand-or-tab)
(define-key global-map (kbd "<escape>") 'vi-on)
(define-key global-map (kbd "<f2>") 'dired-here)
(define-key global-map (kbd "<f3>") 'abbrev-mode)
(define-key global-map (kbd "<f4>") 'variable-pitch-mode)
(define-key global-map (kbd "<f5>") (lambda() (interactive) (switch-to-buffer "*scratch*")))

(define-key prog-mode-map (kbd "<return>") 'newline-and-indent-relative)
(define-key prog-mode-map (kbd "C-SPC") 'unexpand-abbrev)

(define-key text-mode-map (kbd "<return>") 'double-newline)
(define-key text-mode-map (kbd "C-SPC") 'unexpand-abbrev)

(define-key minibuffer-local-map (kbd "<escape>") 'abort-recursive-edit)
(define-key minibuffer-local-map (kbd "<tab>") 'minibuffer-complete)

(define-key isearch-mode-map (kbd "<return>") 'isearch-exit)
(define-key isearch-mode-map (kbd "<escape>") 'isearch-exit)
(define-key isearch-mode-map (kbd "C-g") 'isearch-exit)

(with-eval-after-load 'dired
	(define-key dired-mode-map (kbd "e") 'dired-next-line)
	(define-key dired-mode-map (kbd "o") 'dired-previous-line)
	(define-key dired-mode-map (kbd "i") 'dired-up-directory)
	(define-key dired-mode-map (kbd "/") 'isearch-exit-forward))

(with-eval-after-load 'php-mode
	(define-key php-mode-map (kbd "C-c") nil)
	(define-key php-mode-map (kbd "<tab>") nil)
    (define-key php-mode-map (kbd "d") nil))

(with-eval-after-load 'term
	(define-key term-raw-map (kbd "TAB")
		(lambda() (interactive) (term-send-raw-string (kbd "TAB"))))
	(define-key term-raw-map (kbd "<tab>")
		(lambda() (interactive) (term-send-raw-string (kbd "TAB"))))
	(define-key term-raw-map (kbd "C-c")
		(lambda() (interactive) (term-send-raw-string (kbd "C-c")))))

(with-eval-after-load 'js
	(define-key js-mode-map (kbd "C-c") nil))
