;; -*- lexical-binding: t -*-
(defun whitespacep(x) (member x '(9 10 32)))
(defun pipe(x &rest fs) (seq-reduce (lambda (x f) (funcall f x)) fs x))
(defun arrow(&rest fs) (lambda(x) (pipe x fs)))

(defun map-plist(f xs)
	(let ((head xs) (results ()))
		(while head
			(push (funcall f (car head) (cadr head)) results)
			(setq head (cddr head)))
		(nreverse results)))

(defun backward-whitespace() (interactive) (forward-whitespace -1))

(defun zap-up-to-char-backward() (interactive)
	(zap-up-to-char -1 (read-char "zap to char backward")))

(defun quick-goto-char() (interactive)
	(search-forward (make-string 1 (read-char "go to char"))))

(defun quick-goto-char-backward() (interactive);
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

(defun filedb-walk(root disallowed f)
	(dolist (name (directory-files root))
		(when (not (member name disallowed))
			(let ((pathname (concat root "/" name)))
			(if (file-directory-p pathname)
				(filedb-walk pathname disallowed f)
				(funcall f pathname))))))

(defun update-file-db() (interactive)
	(with-temp-file file-db (filedb-walk
		file-db-root-dir
		file-db-exclude-dirs
		(lambda(x) (insert x "\n")))))

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
	(let (
		(post (read-sexp))
		(cat "")
		(stamp (timestamp))
		(file-name "")
	)
	(setq cat (ido-completing-read "category?> " blog-categories))
	(if (< (buffer-size) 2000)
		(setq post (append (list "post" "@timestamp" stamp "@tag" cat) post))
	(progn
		(setq file-name (concat
			(replace-regexp-in-string " " "_"
				(read-string "file name (no extension): "))
			".html"))
		(setq post (append (list "post" "@filename" file-name "@timestamp" stamp "@tag" cat) post))))
	(insert (pipe post 'vasdown-to-seml 'seml-to-html))))
	;; (cd blog-directory)
	;; (shell-command-this-buffer "python3 ncrender")
	;; (cd "~")))

(defun b-then-a(a b &rest args) (lambda() (interactive) (apply b args) (funcall a)))

(defun read-sexp() (interactive)
	(let ((tokens ()) (c nil))
	(goto-char (point-min))
	(while (not (eobp))
		(setq c (char-after))
		(forward-char 1)
		(when (= 40 c) (push (read-list) tokens)))
	(nreverse tokens)))

(defun read-list()
	(let ((c nil) (flag t) (atoms ()))
	(while flag
		(when (eobp) (throw 'eof "unexpected end of input"))
		(setq c (char-after))
		(cond
			((= 41 c) (forward-char) (setq flag nil))
			((whitespacep c) (forward-char))
			((= 40 c) (forward-char) (push (read-list) atoms))
			(t (push (read-atom) atoms))))
	(nreverse atoms)))

(defun read-atom()
	(let ((c nil) (flag t) (cs ()))
	(while flag
		(when (eobp) (throw 'eof "unexpected end of input"))
		(setq c (char-after))
		(cond
			((whitespacep c) (setq flag nil))
			((= 41 c) (setq flag nil))
			((= 92 c) ; backslash quoting
				(forward-char)
				(push (char-to-string (char-after)) cs)
				(forward-char))
			(t
				(forward-char)
				(push (char-to-string c) cs))))
	(string-join (nreverse cs) "")))

(defun vasdown() (interactive)
	(let ((xml (string-join (mapcar (arrow 'vasdown-to-seml 'seml-to-html) (read-sexp)) "\n")))
	(erase-buffer)
	(insert xml)))

(defun vasdown-to-seml(node)
	(let ((name (car node)) (head (cdr node)) (attrs ()) (children ()))
	(while head
		(cond
			((listp (car head))
				(push (vasdown-to-seml (car head)) children)
				(setq head (cdr head)))
			((string-prefix-p "@" (car head))
				(push (substring (car head) 1) attrs)
				(push (cadr head) attrs)
				(setq head (cddr head)))
			(t
				(push (car head) children)
				(setq head (cdr head)))))
	(list name (nreverse attrs) (nreverse children))))

(defun seml-to-html (elem)
	(let ((tokens ()) (name (nth 0 elem)) (attrs (nth 1 elem)) (children (nth 2 elem)))
	(push "<" tokens)
	(push (car elem) tokens)
	(when attrs
		(push " " tokens)
		(push (string-join (map-plist (lambda (k v) (concat k "=" "\"" v "\"")) attrs) " ") tokens))
	(if children
		(progn
			(push ">" tokens)
			(push
				(string-join
					(mapcar (lambda (x) (cond
						((stringp x) x)
						((listp x) (seml-to-html x))
						(t "")))
						children)
					" ")
				tokens)
			(push (concat "</" name ">") tokens))
		(progn (push "/>" tokens)))
	(string-join (nreverse tokens) "")))
