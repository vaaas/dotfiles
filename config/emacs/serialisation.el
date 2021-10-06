;; -*- lexical-binding: t -*-
(defun read-sexp-from-file(f)
	(with-temp-buffer
		(insert-file-contents f)
		(read-sexp)))

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
	(string-join (nreverse cs))))

(defun vasdown() (interactive)
	(let ((xml (string-join (mapcar
			(lambda (x) (seml-to-html (vasdown-to-seml x)))
			(read-sexp))
		"\n")))
	(erase-buffer)
	(insert xml)))

(defun vasdown-to-seml (elem)
	(let ((name (car elem)) (head (cdr elem)) (attrs ()) (children ()))
	(while head
		(let ((x (car head)))
		(cond
		((listp x) (push (vasdown-to-seml x) children))
		((stringp x)
			(if (string-prefix-p ":" x)
			(progn
				(push (substring x 1) attrs)
				(setq head (cdr head))
				(push (car head) attrs))
			(push (car head) children))))
		(setq head (cdr head))))
	(list name (nreverse attrs) (vasdown-normalise (nreverse children)))))

(defun vasdown-normalise (xs)
	(let ((tokens nil) (str nil))
	(dolist (x xs)
		(cond
		((stringp x)
			(when (or str (and tokens (listp (car tokens)) (not (member (substring x 0 1) (list "." "?" "!" "\"")))))
				(push " " str))
			(push x str))
		((listp x)
			(when str (push " " str))
			(push (string-join (nreverse str)) tokens)
			(setq str nil)
			(push x tokens))))
	(when str (push (string-join (nreverse str)) tokens))
	(nreverse tokens)))

(defun seml-to-html (elem)
	(let ((name (nth 0 elem)) (attrs (nth 1 elem)) (children (nth 2 elem)))
	(string-join (append
		(list "<" name)
		(when attrs (append
			(list " ")
			(intersperse " " (map-plist (lambda (k v) (concat k "=" "\"" v "\"")) attrs))))
		(if children
			(append
				(list ">")
				(mapcar (lambda (x) (if (listp x) (seml-to-html x) x)) children)
				(list "</" name ">"))
			(list "/>"))))))

(defun pp-sexp (elem &optional lvl)
	(unless lvl (setq lvl 0))
	(insert (make-string lvl 9))
	(insert "(")
	(dolist (x elem)
		(cond
		((listp x)
			(insert "\n")
			(pp-sexp x (+ 1 lvl)))
		((stringp x)
			(cond
			((not (member (char-before) (list 10 32 40 41)))
				(insert " "))
			((= (char-before) 41)
				(insert "\n")
				(insert (make-string (+ 1 lvl) 9))))
			(insert x))))
	(insert ")"))
