(setq lexical-binding t)

(defun outside(xs) (lambda(x) (not (member x xs))))

(defun backward-whitespace() (interactive) (forward-whitespace -1))

(defun zap-up-to-char-backward() (interactive)
	(zap-up-to-char -1 (read-char "zap to char backward")))

(defun goto-char() (interactive)
	(search-forward (make-string 1 (read-char "go to char"))))

(defun goto-char-backward() (interactive)
	(search-backward (make-string 1 (read-char "go to char backward"))))

(defun insert-indent()
	(if indent-tabs-mode (insert-char 9) (insert-char 32 tab-width)))

(defun delete-indent()
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

(defun expand-or-tab() (interactive)
	(if (member (char-before) '(9 10 32))
		(insert-indent)
		(call-interactively 'dabbrev-expand)))

(defun line-below() (interactive)
	(end-of-line) (newline-and-indent-relative))

(defun line-above() (interactive)
	(save-excursion
		(beginning-of-line)
		(newline)
		(forward-line -1)
		(indent-relative)))

(defun newline-and-indent-relative() (interactive) (newline) (indent-relative t t))

(defun do-nothing() (interactive))
(put 'do-nothing 'no-self-insert t)

(defun add-trailing-newline() (end-of-buffer) (when (not (= 10 (char-before))) (insert-char 10)))

(defun add-trailing-newline() (end-of-buffer) (when (not (= 10 (char-before))) (insert-char 10)))

(defun filedb-walk(root filter f)
	(dolist (name (directory-files root))
		(when (funcall filter name)
			(setq pathname (concat root "/" name))
			(if (file-directory-p pathname)
				(filedb-walk pathname filter f)
				(funcall f pathname)))))

(defun update-file-db() (interactive)
	(setq disallowed '("." ".." "node_modules" ".git" "public" "vendor"))
	(with-temp-file file-db
		(filedb-walk "a:/code" (outside disallowed) (lambda(x) (insert x "\n")))))

(defun quick-find-file() (interactive)
	(find-file
	(ido-completing-read "select file> "
	(split-string
	(slurp file-db)
	"\n"))))

(defun slurp(f)
	(with-temp-buffer
		(insert-file-contents f)
		(buffer-substring-no-properties (point-min) (point-max))))

(defun dired-here() (interactive) (dired default-directory))

(defun vi-on() (interactive)
	(vi-mode 1)
	(setq cursor-type 'box))

(defun vi-off() (interactive)
	(vi-mode -1)
	(setq cursor-type 'bar))

(defun backspace-or-unindent() (interactive)
	(cond
		((use-region-p) (call-interactively 'kill-region))
		((< (point) (+ 1 tab-width)) (backward-delete-char 1))
		(indent-tabs-mode (backward-delete-char 1))
		((string= (make-string tab-width 32) (buffer-substring (point) (- (point) tab-width)))
			(backward-delete-char tab-width))
		(t (backward-delete-char 1))))

(defun double-newline() (interactive)
	(cond
		((= 0 (point)) (newline))
		((= 10 (char-before)) (newline))
		(t (newline) (newline))))

(defun toggle-indent-tabs() (interactive)
	(if indent-tabs-mode
		(progn (setq indent-tabs-mode nil) (setq tab-width 4) (message "indent will use SPACES"))
		(progn (setq indent-tabs-mode t) (setq tab-width 3) (message "indent will use TABS"))))

(defun replace-all (from to)
	(beginning-of-buffer)
	(while (search-forward from nil t)
		(replace-match to t t)))

(defun replace-all-regex (from to)
	(beginning-of-buffer)
	(while (re-search-forward from nil t)
		(replace-match to t nil)))

(defun french() (interactive)
	(replace-all-regex "\"\\([^\"]+?\\)\"" "« \\1 »")
	(replace-all "?!" "⁈")
	(replace-all-regex "[ |\u202F]+\\(;\\|!\\|\\?\\|⁈\\)" "\u202F\\1")
	(replace-all-regex "[ |\u00A0]+\\([:\\|»]\\)" "\u00A0\\1")
	(replace-all-regex "«[ |\u00A0]+" "«\u00A0"))

(defun eval-region-smart() (interactive)
	(shell-command-on-region (point-min) (point-max) eval-process))

(defun shell-command-this-buffer(x) (interactive)
	(shell-command-on-region (point-min) (point-max) x (current-buffer) t))

(defun cmark() (interactive)
	(shell-command-this-buffer "cmark --smart --unsafe"))

(defun spaces-to-tabs() (interactive)
	(beginning-of-buffer)
	(replace-all (make-string tab-width 32) "	"))

(defun join-line() (interactive)
	(end-of-line)
	(forward-line)
	(beginning-of-line-text)
	(delete-indentation))

(defun indent-line-or-region() (interactive)
	(if (use-region-p)
		(progn
			(call-interactively 'indent-rigidly-right-to-tab-stop)
			(setq deactivate-mark nil))
		(save-excursion (beginning-of-line) (insert-indent))))

(defun unindent-line-or-region() (interactive)
	(if (use-region-p)
		(progn
			(call-interactively 'indent-rigidly-left-to-tab-stop)
			(setq deactivate-mark nil))
		(save-excursion (beginning-of-line) (delete-indent))))

(defun timestamp() (format-time-string "%s"))

(defun blog() (interactive)
	(setq cat (ido-completing-read "category?> "
		'("tech" "anime" "books" "memes" "films" "journal" "games")))
	(setq stamp (timestamp))
	(delete-trailing-whitespace)
	(beginning-of-buffer)
	(if (< (buffer-size) 2000)
		(insert (format "\n%s\n%s\n" stamp cat))
	(progn
		(search-forward "<h1>") (setq start (point))
		(search-forward "</h1>") (setq end (- (point) 5))
		(setq title (buffer-substring start end))
		(search-forward "<p>") (setq start (point))
		(search-forward "</p>") (setq end (- (point) 4))
		(setq blurb (buffer-substring start end))
		(setq file-name (concat
			(replace-regexp-in-string " " "_"
				(read-string "file name (no extension): "))
			".html"))
		(shell-command-this-buffer (concat blog-directory "/ncrender -s"))
		(append-to-file (point-min) (point-max) (concat blog-directory "/render/" file-name))
		(kill-region (point-min) (point-max))
		(insert
			(format "\n%s\n%s\n<h1><a href=\"/%s\">%s</a></h1>\n%s"
			stamp cat file-name title blurb))))
	(add-trailing-newline)
	(append-to-file (point-min) (point-max) (concat blog-directory "/posts")))

(defun next-buffer-and-vi-on() (interactive) (next-buffer) (vi-on))
(defun previous-buffer-and-vi-on() (interactive) (previous-buffer) (vi-on))
