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
	(string-join (nreverse cs) "")))

(defun vasdown() (interactive)
	(let ((xml (string-join (mapcar 'vasdown-to-html (read-sexp)) "\n")))
	(erase-buffer)
	(insert xml)))

(defun vasdown-to-html (elem)
	(let ((name (car elem)) (head (cdr elem)) (attrs ()) (children ()))
	(while head
		(cond
			((listp (car head)) (push (vasdown-to-html (car head)) children))
			((stringp (car head))
				(if (string-prefix-p ":" (car head))
				(progn
					(push (substring (car head) 1) attrs)
					(setq head (cdr head))
					(push (car head) attrs))
				(push (car head) children))))
		(setq head (cdr head)))
	(string-join
		(list
			"<" name
			(if attrs
				(string-join (cons "" (map-plist (lambda (k v) (concat k "=" "\"" v "\"")) (nreverse attrs))) " ")
				"")
			(if children
				(string-join (list ">" (string-join (nreverse children) " ") "</" name ">") "")
				"/>"))
		"")))
