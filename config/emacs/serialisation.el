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
			((string-prefix-p ":" (car head))
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
