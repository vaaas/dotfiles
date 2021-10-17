; -*- lexical-binding: t -*-
; this file defines vas-normal-mode, which is a modal text editing mode

(defvar-local vas-normal-bonus-mode nil)

(defun vas-normal-mode-on()
	"turn on vas-normal-mode and change the cursor shape."
	(interactive)
	(vas-normal-mode 1)
	(when vas-normal-bonus-mode (funcall vas-normal-bonus-mode 1))
	(setq cursor-type 'box))

(defun vas-normal-mode-off()
	"turn off vas-normal-mode and change the cursor shape."
	(interactive)
	(vas-normal-mode -1)
	(when vas-normal-bonus-mode (funcall vas-normal-bonus-mode -1))
	(setq cursor-type 'bar))

(defvar vas-normal-mode-map (define-new-keymap (alist
	"x" 'execute-extended-command
	"X" 'eval-expression
	"c" 'kill-whole-line
	"C" 'kill-line
	"d" 'backward-kill-word
	"D" 'kill-word
	"m" 'indent-line-or-region
	"M" 'unindent-line-or-region
	"j" 'join-line
	"J" 'comment-or-uncomment-region
	"q" (lambda() (interactive) (kill-this-buffer) (vas-normal-mode-on))
	"Q" 'save-buffers-kill-terminal
	"w" 'forward-word
	"W" 'forward-whitespace
	"u" 'backward-word
	"U" 'backward-whitespace
	"v" 'goto-line
	";" (lambda() (interactive) (end-of-line) (insert-char #x3B))
	"`" 'eshell

	"r" 'zap-up-to-char
	"R" 'zap-up-to-char-backward
	"s" (lambda() (interactive) (if (use-region-p) (deactivate-mark) (call-interactively 'set-mark-command)))
	"S" (lambda() (interactive) (beginning-of-line) (call-interactively 'set-mark-command) (forward-line))
	"t" 'quick-goto-char
	"T" 'quick-goto-char-backward
	"n" 'backspace-or-unindent
	"N" 'delete-forward-char
	"h" 'scroll-up-command
	"H" 'scroll-down-command
	"y" 'isearch-repeat-forward
	"Y" 'isearch-repeat-backward
	"i" 'left-char
	"I" 'beginning-of-line-text
	"e" 'next-line
	"E" 'forward-paragraph
	"o" 'previous-line
	"O" 'backward-paragraph
	"a" 'right-char
	"A" 'end-of-line

	"z" 'undo
	"Z" 'keyboard-quit
	"f" 'find-file
	"F" 'filedb-find-file
	"g" 'beginning-of-buffer
	"G" 'end-of-buffer
	"l" 'line-below
	"L" 'line-above
	"k" 'kmacro-start-macro
	"K" 'kmacro-end-or-call-macro
	"b" 'ido-switch-buffer
	"B" 'save-buffer
	"p" 'yank
	"P" 'kill-ring-save
	"." 'repeat
	"<" 'outline-hide-entry
	">" 'outline-show-entry
	"/" 'isearch-forward
	"?" 'isearch-backward
	"SPC" 'vas-normal-mode-off
	"<escape>" 'keyboard-quit
	"(" (lambda() (interactive) (next-buffer) (vas-normal-mode-on))
	")" (lambda() (interactive) (previous-buffer) (vas-normal-mode-on))
	"<tab>" (lambda() (interactive) (switch-to-buffer nil) (vas-normal-mode-on))
	"TAB" (lambda() (interactive) (switch-to-buffer nil) (vas-normal-mode-on))

	"' v p" 'variable-pitch-mode
	"' e a" (lambda() (interactive) (find-file (concat user-emacs-directory "/abbrev_defs")))
	"' e i" (lambda() (interactive) (find-file user-init-file))
	"' i t" 'toggle-indent-tabs
	"' f r" 'french
	"' c m" 'cmark
	"' s t" 'spaces-to-tabs
	"' e r" 'eval-region
	"' e b" 'eval-buffer
	"' s s" 'delete-other-windows
	"' i b" 'ibuffer
	"' c w" 'count-words
	"' A" 'mark-whole-buffer
	"' q w" (lambda() (interactive) (split-window-horizontally) (split-window-horizontally) (balance-windows) (follow-mode))
	"' o f" 'outline-hide-body
	"' o u" 'outline-show-all

	"' m m" 'markdown-mode
	"' m j" 'js-mode
	"' m p" 'php-mode))

	"keymap for vas-normal-mode")

(define-minor-mode vas-normal-mode
	"Ghetto vi normal mode."
	:lighter " vas"
	:keymap 'vas-normal-mode-map)
