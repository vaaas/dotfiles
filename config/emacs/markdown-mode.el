; -*- lexical-binding: t -*-
(defvar vas-markdown-code-modes
	(alist 'javascript #'js-mode
		'js #'js-mode
		'vue #'html-mode
		'php #'php-mode
		'elisp #'emacs-lisp-mode
		'html #'html-mode
		'xml #'xml-mode)
	"A mapping of code block names to emacs modes.")

(defun vas-markdown-edit-code ()
	"Edit the code block the cursor is at in a different buffer."
	(interactive)
	(let*
		((start (progn (search-backward "```") (point)))
		(eofl (progn (end-of-line) (point)))
		(end (progn (search-forward "```") (point)))
		(mode (->
			(buffer-substring (+ 3 start) eofl)
			(string-trim $)
			(split-string $ "\s+")
			(car $)
			(intern $)
			(alist-get $ vas-markdown-code-modes)
			(or $ 'prog-mode))))
	(edit-buffer-region "*markdown-code-narrow-indirect*" (+ eofl 1) (- end 3) (call-interactively mode))))

(define-minor-mode markdown-normal-mode
	"markdown-mode extensions for vas-normal-mode"
	:keymap (define-new-keymap (alist
		"<return>" #'vas-markdown-edit-code)))

(with-eval-after-load 'markdown-mode
	(define-key markdown-mode-map (kbd "<return>") #'newline-and-indent-relative)
	(define-key markdown-mode-map (kbd "C-c '") #'vas-markdown-edit-code)
	(add-hook 'markdown-mode-hook (lambda ()
		(when vas-normal-mode (markdown-normal-mode))
		(setq-local vas-normal-bonus-mode #'markdown-normal-mode))))
