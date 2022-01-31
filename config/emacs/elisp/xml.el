; -*- lexical-binding: t -*-
; helper functions for dealing with XML data in emacs. These assume the data structure as returned from libxml-parse-xml-region.

(defun read-xml-file (file)
	"parse the the XML file FILE. see `libxml-parse-xml-region'"
	(with-current-buffer (find-file-noselect file)
	(libxml-parse-xml-region (point-min) (point-max))))

(defun xml-elem= (s) (L x (and (listp x) (eq s (car x)))))

(defun xml-to-string (node)
	(cond
	((eq nil node) "")
	((stringp node) (xml-escape-string node))
	((numberp node) (number-to-string node))
	((symbolp node) (symbol-name node))
	((listp node) (xml-node-to-string node))
	(t "")))

(defun xml-node-to-string (node)
	(let
		((name (car node))
		(attrs (cadr node))
		(children (cddr node)))
	(cond
	((eq 'comment name)
		(concat "<!--" (string-join children " ") "-->"))
	((eq '!cdata name)
		(string-join (mapcar (L x (xml-escape-string (xml-to-string x))) children)))
	(t (concat
		"<"
		(symbol-name name)
		(if attrs (concat " " (xml-serialise-attrs attrs)) "")
		(if children
			(concat
				">"
				(string-join (mapcar #'xml-to-string children))
				"</" (symbol-name name) ">")
			"/>"))))))

(defun xml-inner-text (node)
	(-> (cddr node)
		(mapcar (L x (if (stringp x) x (xml-inner-text x))) $)
		(string-join $)))

(defun xml-serialise-attrs (xs)
	(C string-join " "
		(C map-alist xs
			(lambda (k v)
				(concat (symbol-name k) "=" "\"" (xml-to-string v) "\"")))))

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
