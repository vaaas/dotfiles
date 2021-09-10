(define-key global-map (kbd "C-x") 'execute-extended-command)
(define-key global-map (kbd "C-S-X") 'eval-expression)
(define-key global-map (kbd "C-c") 'kill-whole-line)
(define-key global-map (kbd "C-S-C") 'kill-line)
(define-key global-map (kbd "C-d") 'backward-kill-word)
(define-key global-map (kbd "C-S-D") 'kill-word)
(define-key global-map (kbd "C-m") 'indent-line-or-region)
(define-key global-map (kbd "C-S-M") 'unindent-line-or-region)
(define-key global-map (kbd "C-j") 'join-line)
(define-key global-map (kbd "C-S-J") 'comment-or-uncomment-region)
(define-key global-map (kbd "C-q") 'kill-this-buffer)
(define-key global-map (kbd "C-S-Q") 'save-buffers-kill-terminal)
(define-key global-map (kbd "C-w") 'forward-word)
(define-key global-map (kbd "C-S-W") 'forward-whitespace)
(define-key global-map (kbd "C-u") 'backward-word)
(define-key global-map (kbd "C-S-U") 'backward-whitespace)
(define-key global-map (kbd "C-v") 'goto-line)
(define-key global-map (kbd "C-;") (lambda() (interactive) (end-of-line) (insert-char #x3B)))
(define-key global-map (kbd "C-`") 'eshell)

(define-key global-map (kbd "C-r") 'zap-up-to-char)
(define-key global-map (kbd "C-S-R") 'zap-up-to-char-backward)
(define-key global-map (kbd "C-s") (lambda() (interactive) (if (use-region-p) (deactivate-mark) (call-interactively 'set-mark-command))))
(define-key global-map (kbd "C-S-S") (lambda() (interactive) (beginning-of-line) (call-interactively 'set-mark-command) (forward-line)))
(define-key global-map (kbd "C-t") 'quick-goto-char)
(define-key global-map (kbd "C-S-T") 'quick-goto-char-backward)
(define-key global-map (kbd "C-n") 'backspace-or-unindent)
(define-key global-map (kbd "C-S-N") 'delete-forward-char)
(define-key global-map (kbd "C-h") 'scroll-up-command)
(define-key global-map (kbd "C-S-H") 'scroll-down-command)
(define-key global-map (kbd "C-y") 'isearch-repeat-forward)
(define-key global-map (kbd "C-S-Y") 'isearch-repeat-backward)
(define-key global-map (kbd "C-i") 'left-char)
(define-key global-map (kbd "C-S-I") 'beginning-of-line-text)
(define-key global-map (kbd "C-e") 'next-line)
(define-key global-map (kbd "C-S-E") 'forward-paragraph)
(define-key global-map (kbd "C-o") 'previous-line)
(define-key global-map (kbd "C-S-O") 'backward-paragraph)
(define-key global-map (kbd "C-a") 'right-char)
(define-key global-map (kbd "C-S-A") 'end-of-line)

(define-key global-map (kbd "C-f") 'find-file)
(define-key global-map (kbd "C-S-F") 'quick-find-file)
(define-key global-map (kbd "C-g") 'beginning-of-buffer)
(define-key global-map (kbd "C-S-G") 'end-of-buffer)
(define-key global-map (kbd "C-l") 'line-below)
(define-key global-map (kbd "C-S-L") 'line-above)
(define-key global-map (kbd "C-k") 'kmacro-start-macro)
(define-key global-map (kbd "C-S-K") 'kmacro-end-or-call-macro)
(define-key global-map (kbd "C-b") 'ido-switch-buffer)
(define-key global-map (kbd "C-S-B") 'save-buffer)
(define-key global-map (kbd "C-p") 'yank)
(define-key global-map (kbd "C-S-P") 'kill-ring-save)

(define-key global-map (kbd "<backspace>") 'backspace-or-unindent)
(define-key global-map (kbd "<tab>") 'expand-or-tab)
(define-key global-map (kbd "<escape>") 'vi-on)
(define-key global-map (kbd "<f2>") 'dired-here)
(define-key global-map (kbd "<f3>") 'abbrev-mode)
(define-key global-map (kbd "<f4>") 'variable-pitch-mode)
(define-key global-map (kbd "<f5>") (lambda() (interactive) (switch-to-buffer "*scratch*")))

(define-key prog-mode-map (kbd "<return>") 'newline-and-indent-relative)
(define-key prog-mode-map (kbd "C-SPC") 'unexpand-abbrev)

(define-key text-mode-map (kbd "C-SPC") 'unexpand-abbrev)

(define-key minibuffer-local-map (kbd "<escape>") 'abort-recursive-edit)
(define-key minibuffer-local-map (kbd "<tab>") 'minibuffer-complete)

(define-key isearch-mode-map (kbd "<return>") 'isearch-exit)
(define-key isearch-mode-map (kbd "<escape>") 'isearch-exit)
(define-key isearch-mode-map (kbd "C-g") 'isearch-exit)

(with-eval-after-load 'markdown-mode
	(define-key markdown-mode-map (kbd "<return>") 'double-newline)
	(define-key markdown-mode-map (kbd "C-n") 'double-newline))

(with-eval-after-load 'dired
	(define-key dired-mode-map (kbd "e") 'dired-next-line)
	(define-key dired-mode-map (kbd "o") 'dired-previous-line)
	(define-key dired-mode-map (kbd "i") 'dired-up-directory)
	(define-key dired-mode-map (kbd "/") 'isearch-exit-forward))

(with-eval-after-load 'php-mode
	(define-key php-mode-map (kbd "<tab>") nil))

(with-eval-after-load 'term
	(define-key term-raw-map (kbd "TAB")
		(lambda() (interactive) (term-send-raw-string (kbd "TAB"))))
	(define-key term-raw-map (kbd "C-c")
		(lambda() (interactive) (term-send-raw-string (kbd "C-c")))))
