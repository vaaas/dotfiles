;; -*- lexical-binding: t -*-
(defun whitespacep(x) (member x '(9 10 32)))
(defun timestamp() (format-time-string "%s"))
(defun plist-to-alist (xs) (map-plist #'cons xs))
(defun alist (&rest xs) (plist-to-alist xs))
(defun xml-elem= (s) (lambda (x) (and (listp x) (= s (car x)))))

(defun find(f xs)
	(let ((x xs) (found nil))
	(while (and x (not found))
		(when (funcall f (car x)) (setq found (car x)))
		(setq x (cdr x)))
	found))

(defun query-selector (f node)
	(cond
	((funcall f node) node)
	((listp node)
		(let ((x (cddr node)))
		(while (and x (not (query-selector(f x))))
			(setq x (cdr x)))
		x))
	(t nil)))

(defun query-selector-all (f node)
	(let
		((xs (when (funcall f node) node))
		(x (when (listp node) (cddr node))))
	(while x
		(setcdr (last xs) (query-selector-all f x))
		(setq x (cdr x)))
	xs))

(defun map-plist(f xs)
	(let ((head xs) (results ()))
	(while head
		(push (funcall f (car head) (cadr head)) results)
		(setq head (cddr head)))
	(nreverse results)))

(defun slurp(f)
	(with-temp-buffer
		(insert-file-contents f)
		(buffer-substring-no-properties (point-min) (point-max))))

(defun intersperse (s xs)
	(let ((r ()))
	(push (car xs) r)
	(dolist (x (cdr xs)) (push s r) (push x r))
	(nreverse r)))

(defun serialise-xml(node)
	(cond
	((stringp node) (xml-escape-string node))
	((listp node) (serialise-xml-node))))

(defun serialise-xml-node(node)
	(let
		((name (symbol-name (car node)))
		(attrs (cadr node))
		(children (cddr node)))
	(string-join
		(if (string= "comment" name)
			(list "<!--" (car children) "-->")
		(append
			(list "<" name)
			(when attrs (append (list " ") (tokenise-xml-attrs attrs)))
			(if children
				(append
					(list ">")
					(mapcar 'serialise-xml children)
					(list "</" name ">"))
				(list "/>")))))))

(defun tokenise-xml-attrs (xs)
	(apply #'append
	(intersperse (list " ")
	(mapcar (lambda (x) (list (symbol-name (car x)) "=" "\"" (cdr x) "\""))
	xs))))

(defmacro with-temp-dir (temp-dir &rest body)
	`(let ((old default-directory))
	(cd ,temp-dir)
	(unwind-protect (progn ,@body) (cd old))))

(defun read-elisp-file (file)
	(with-current-buffer (find-file-noselect file)
	(goto-char (point-min))
	(read (current-buffer))))
