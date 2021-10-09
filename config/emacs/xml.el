;;; -*- lexical-binding: t -*-
(defun xml-elem= (s) (lambda (x) (when (listp x) (eq s (car x)))))

(defun serialise-xml(node)
	(cond
	((stringp node) (xml-escape-string node))
	((listp node) (serialise-xml-node node))))

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

(defun xml-inner-text (node)
	(string-join
	(mapcar (lambda (x) (if (stringp x) x (xml-inner-text x)))
	(cddr node))))

(defun tokenise-xml-attrs (xs)
	(apply #'append
	(intersperse (list " ")
	(mapcar (lambda (x) (list (symbol-name (car x)) "=" "\"" (cdr x) "\""))
	xs))))

(defun query-selector (f node)
	(cond
	((funcall f node) node)
	((listp node)
		(let ((x (cddr node)) (found nil))
		(while (and x (not found))
			(setq found (query-selector f (car x)))
			(setq x (cdr x)))
		found))
	(t nil)))

(defun query-selector-all (f node)
	(let ((xs (when (funcall f node) (list node))))
	(dolist (x (when (listp node) (cddr node)))
		(dolist (y (query-selector-all f x))
			(push y xs)))
	(nreverse xs)))
