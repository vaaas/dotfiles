; -*- lexical-binding: t -*-
(defun backward-whitespace()
	"Move point to end of the previous sequence of whitespace whitespace chars. The same as `forward-whitespace' with negative argument."
	(interactive)
	(forward-whitespace -1))

(defun zap-up-to-char-backward()
	"`zap-up-to-char' with negative argument"
	(interactive)
	(zap-up-to-char -1 (read-char "zap to char backward")))

(defun quick-goto-char()
	"Search forward to the character read from the user."
	(interactive)
	(search-forward (make-string 1 (read-char "go to char"))))

(defun quick-goto-char-backward()
	"Like `quick-goto-char' but backward."
	(interactive);
	(search-backward (make-string 1 (read-char "go to char backward"))))

(defun insert-indent()
	"Insert a tab character, or a number of spaces equal to `tab-width', depending on whether `indent-tabs-mode' is set."
	(if indent-tabs-mode (insert-char 9) (insert-char 32 tab-width)))

(defun delete-indent()
	"Delete the previous indent. Depending on `indent-tabs-mode', the indent may be a tab character or spaces equal to `tab-width'."
	(cond
		((and indent-tabs-mode (= 9 (char-after)))
			(delete-forward-char 1))
		((and
			(not indent-tabs-mode)
			(string=
				(make-string tab-width 32)
				(buffer-substring (point) (+ (point) tab-width))))
			(delete-forward-char tab-width))
		(t (do-nothing))))

(defun expand-or-tab()
	"`insert-indent', or `dabbrev-expand', depending on whether `char-befor' is whitespace or not"
	(interactive)
	(if (whitespacep (char-before))
		(insert-indent)
		(call-interactively 'dabbrev-expand)))

(defun line-below()
	"Create a new empty line below the current one, and indent it."
	(interactive)
	(end-of-line) (newline-and-indent-relative))

(defun line-above()
	"Like `line-below', but for above."
	(interactive)
	(save-excursion
		(beginning-of-line)
		(newline)
		(forward-line -1)
		(indent-relative)))

(defun newline-and-indent-relative()
	"Create a new line and indent it with the same degree of indent as the line above it."
	(interactive) (newline) (indent-relative t t))

(defun do-nothing() "Do nothing." (interactive))
(put 'do-nothing 'no-self-insert t)

(defun dired-here()
	"Open a dired buffer in the current file's directory."
	(interactive) (dired default-directory))

(defun backspace-or-unindent()
	"Delete the character before the cursor, or unindent the line. Additionally, if the region is active, kill the region."
	(interactive)
	(cond
		((use-region-p) (call-interactively 'kill-region))
		((< (point) (+ 1 tab-width)) (backward-delete-char 1))
		(indent-tabs-mode (backward-delete-char 1))
		((string= (make-string tab-width 32) (buffer-substring (point) (- (point) tab-width)))
			(backward-delete-char tab-width))
		(t (backward-delete-char 1))))

(defun double-newline()
	"Insert two newlines. Useful for writing, where paragraphs are double spaced."
	(interactive)
	(cond
		((= 0 (point)) (newline))
		((= 10 (char-before)) (newline))
		(t (newline) (newline))))

(defun toggle-indent-tabs()
	"Toggle between spaces and tabs indent mode."
	(interactive)
	(if indent-tabs-mode
		(progn (setq indent-tabs-mode nil) (setq tab-width 4) (message "indent will use SPACES"))
		(progn (setq indent-tabs-mode t) (setq tab-width 3) (message "indent will use TABS"))))

(defun replace-all (from to)
	"Replace all strings in buffer from FROM to TO."
	(beginning-of-buffer)
	(while (search-forward from nil t)
		(replace-match to t t)))

(defun replace-all-regex (from to)
	"Replace all strings in buffer matching FROM to tO."
	(beginning-of-buffer)
	(while (re-search-forward from nil t)
		(replace-match to t nil)))

(defun french()
	"Space french text appropriately. See URL `https://unicode.org/udhr/n/notes_fra.html'"
	(interactive)
	(replace-all-regex "\"\\([^\"]+?\\)\"" "« \\1 »")
	(replace-all "?!" "⁈")
	(replace-all-regex "[ |\u202F]+\\(;\\|!\\|\\?\\|⁈\\)" "\u202F\\1")
	(replace-all-regex "[ |\u00A0]+\\([:\\|»]\\)" "\u00A0\\1")
	(replace-all-regex "«[ |\u00A0]+" "«\u00A0")
	(replace-all "  " " "))

(defvar-local eval-process "cat"
	"Process name that smart eval should use. By default, just print the buffer contents with cat. Each mode should set its own process.")

(defun eval-region-smart()
	"Eval region using the buffer's local variable `eval-process'."
	(interactive)
	(shell-command-on-region (point-min) (point-max) eval-process))

(defun shell-command-this-buffer(x)
	"Run shell command X on this entire buffer."
	(interactive)
	(shell-command-on-region (point-min) (point-max) x (current-buffer) t))

(defun cmark()
	"Run commanmark (cmark) on this entire buffer."
	(interactive)
	(shell-command-this-buffer "cmark --smart --unsafe"))

(defun spaces-to-tabs()
	"Replace spaces to tabs for this buffer."
	(interactive)
	(beginning-of-buffer)
	(replace-all (make-string tab-width 32) "	"))

(defun join-line()
	"Join this line with the next line."
	(interactive)
	(end-of-line)
	(forward-line)
	(beginning-of-line-text)
	(delete-indentation))

(defun indent-line-or-region()
	"Increase the region's indent level by one. If there is no region, indent just the current line."
	(interactive)
	(if (use-region-p)
		(progn
			(call-interactively 'indent-rigidly-right-to-tab-stop)
			(setq deactivate-mark nil))
		(save-excursion (beginning-of-line) (insert-indent))))

(defun unindent-line-or-region()
	"Decrease the region's indent level by one. If there is no region, unindent just the current line."
	(interactive)
	(if (use-region-p)
		(progn
			(call-interactively 'indent-rigidly-left-to-tab-stop)
			(setq deactivate-mark nil))
		(save-excursion (beginning-of-line) (delete-indent))))

(defvar vas-markdown-code-modes
	(alist 'javascript 'js-mode
		'js 'js-mode
		'vue 'html-mode
		'php 'php-mode
		'elisp 'emacs-lisp-mode
		'html 'html-mode
		'xml 'xml-mode)
	"A mapping of code block names to emacs modes.")

(defun vas-markdown-edit-code ()
	"Edit the code block the cursor is at in a different buffer."
	(interactive)
	(save-excursion
	(let*
		((start (progn (search-backward "```") (point)))
		(eofl (progn (end-of-line) (point)))
		(end (progn (search-forward "```") (point)))
		(contents (buffer-substring (+ 1 eofl) (- end 3)))
		(mode
			(or
				(alist-get
					(intern (car (split-string (string-trim (buffer-substring (+ 3 start) eofl)) "\s+")))
					vas-markdown-code-modes)
				'prog-mode)))
	(edit-buffer-region "*markdown-code-narrow-indirect*" (+ eofl 1) (- end 3) (call-interactively mode)))))
