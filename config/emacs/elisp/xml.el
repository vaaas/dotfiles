; -*- lexical-binding: t -*-
; helper functions for dealing with XML data in emacs. These assume the data structure as returned from libxml-parse-xml-region.

(defun read-xml-file (file)
	"parse the the XML file FILE. see `libxml-parse-xml-region'"
	(with-current-buffer (find-file-noselect file)
	(libxml-parse-xml-region (point-min) (point-max))))

(defun xml-elem= (s) (lambda (x) (when (listp x) (eq s (car x)))))

(defun serialise-xml(node)
	(cond
	((eq nil node) "")
	((stringp node) (xml-escape-string node))
	((listp node) (serialise-xml-node node))
	(t "")))

(defun serialise-xml-node(node)
	(let
		((name (car node))
		(attrs (cadr node))
		(children (cddr node)))
	(string-join
	(cond
	((eq 'comment name) (list "<!--" (car children) "-->"))
	((eq '!cdata name) (mapcar (=> (serialise-xml $) (xml-escape-string $)) children))
	(t (append
		(list "<" (symbol-name name))
		(when attrs (append (list " ") (tokenise-xml-attrs attrs)))
		(if children
			(append
				(list ">")
				(mapcar #'serialise-xml children)
				(list "</" (symbol-name name) ">"))
			(list "/>"))))))))

(defun xml-inner-text (node)
	(string-join
	(mapcar (lambda (x) (if (stringp x) x (xml-inner-text x)))
	(cddr node))))

(defun tokenise-xml-attrs (xs)
	(-> xs
	(map-alist
		(lambda (k v)
			(-> (cond
				((numberp v) (number-to-string v))
				((stringp v) (xml-escape-string v))
				((symbolp v) (symbol-name v))
				(t ""))
			(list (symbol-name k) "=" "\"" $ "\"")))
		$)
	(intersperse (list " ") $)
	(apply #'append $)))

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
	(nreverse
	(dolist (x (when (listp node) (cddr node)) xs)
		(push-all (query-selector-all f x) xs)))))
