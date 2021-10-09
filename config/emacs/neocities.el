;;; -*- lexical-binding: t -*-
(defun nc-render() (interactive)
	(let
		((site (read-elisp-file (concat blog-directory "/site.lisp")))
		(posts nil)
		(doctype "<!DOCTYPE html>"))
	(setq posts (alist-get 'posts site))
	; render index.html
	;; (with-temp-file (concat blog-directory "/render/index.html")
	;; 	(insert
	;; 	(concat doctype
	;; 	(serialise-xml
	;; 	(nc-frontpage site
	;; 	(mapcar #'nc-render-item
	;; 	(seq-filter (lambda (x) (not (alist-get 'skip (nth 1 x))))
	;; 	posts)))))))

	; render rss.xml
	(with-temp-file (concat blog-directory "/render/rss.xml")
		(insert
		(concat "<?xml version='1.0' encoding='UTF-8'?>"
		(serialise-xml
		(nc-rss site
		(mapcar (lambda (x) (nc-rss-item site x))
		(seq-filter (lambda (x) (not (alist-get 'skip (nth 1 x))))
		posts)))))))

	; render individual articles
	;; (dolist (x (seq-filter (lambda(x) (alist-get 'filename (nth 1 x))) posts))
	;; 	(with-temp-file (concat blog-directory "/render"/ (alist-get 'filename (nth 1 x)))
	;; 		(insert (concat doctype (serialise-xml (nc-post x))))))
))

(defun nc-render-item (x)
	(append
		(list 'article (alist
			't (alist-get 'tag (nth 1 x))
			'id (nc-guid (alist-get 'timestamp (nth 1 x)))))
		(nc-description x)))

(defun nc-description (x)
	(let ((filename (alist-get 'filename (nth 1 x))))
	(append
		(list (list 'time nil (nc-ymd (alist-get 'timestamp (nth 1 x)))))
		(if filename
		(let
			((h1 (query-selector (xml-elem= 'h1) x))
			(p (query-selector (xml-elem= 'p) x)))
			(when (not h1) (throw 'bad-post "No h1 found"))
			(when (not p) (throw 'bad-post "No p found"))
			(list
				(append
					(list 'h1 nil (list 'a (alist 'href (concat "/" filename))))
					(cddr h1))
				(cddr p)))
		(cddr x)))))

(defun nc-frontpage (conf xs)
	(let
	((distinct-tags
		(sort
		(delete-dups
		(mapcar (lambda (x) (alist-get 't (nth 1 x))) xs))
		#'string<)))
	(nc-html
		(alist-get 'lang conf)
		(nc-head conf (alist-get 'sitename conf))
		(list 'body nil
			(list 'header nil
				(list 'div (alist 'class "imgtxt")
					(list 'img (alist 'src "/pics/banner.jpg"))
					(list 'h1 nil (alist-get 'sitename conf)))
				(alist-get 'blurb conf))
			(append
				(list 'nav nil)
				(alist-get 'links conf)
				(list (list 'a (alist 'class "active" 'href "all") "all"))
				(mapcar (lambda (x) (list 'a (alist 'href x) x)) distinct-tags))
			(append (list 'main nil) (nreverse xs))
			(list 'script (alist 'src "/script.js") " ")))))

(defun nc-head (conf title)
	(list 'head nil
		(list 'meta (alist 'charset "utf8"))
		(list 'meta (alist 'name "viewport" 'content "width=device-width, initial-scale=1.0"))
		(list 'meta (alist 'name "url" 'content (alist-get 'url conf)))
		(list 'meta (alist 'name "author" 'content (alist-get 'author conf)))
		(list 'meta (alist 'name "description" 'content (alist-get 'sitename conf)))
		(list 'link (alist 'rel "stylesheet" 'href "/style.css"))
		(list 'link (alist 'rel "icon" 'href "/favicon.ico"))
		(list 'link (alist 'rel "alternate" 'href "/rss.xml" 'type "application/rss+xml"))
		(list 'title nil title)))

(defun nc-html(lang head body) (list 'html (alist 'lang lang) head body))
(defun nc-guid (x) "43892174312") ;todo
(defun nc-ymd (x) "6666-69-42") ;todo
(defun nc-rfctime (x) "Today is nice") ;todo

(defun nc-rss-item (site x)
	(let ((guid nil) (url nil))
	(setq guid (nc-guid (alist-get 'timestamp x)))
	(setq url (concat (alist-get 'url site) "/#" guid))
	(list 'item nil
		(list 'title nil (xml-inner-text x))
			(iff (query-selector (xml-elem= 'h1) x)
				#'xml-inner-text
				(K (format "New post by %s (%s)" (alist-get 'author site) guid))))
		(list 'guid nil url)
		(list 'pubDate nil (nc-rfctime (alist-get 'timestamp x)))
		(list 'link nil url)
		(append
			(list 'description nil)
			(mapcar (lambda (x) (xml-escape-string (serialise-xml x))) (nc-description x)))))

(defun nc-rss (site xs)
	(list 'rss (alist 'version "2.0" 'xmlns:atom "http://www.w3.org/2005/Atom")
		(append
			(list 'channel nil
				(list 'title nil (alist-get 'sitename site))
				(list 'link nil (alist-get 'url site))
				(list 'atom:link
					(alist 'href (concat (alist-get 'url site) "/rss.xml")
						'rel "self"
						'type "application/rss+xml"))
				(list 'description nil (alist-get 'sitename site))
				(list 'pubDate nil (xml-inner-text (query-selector (xml-elem= 'pubDate) (car xs))))
				(list 'language nil (alist-get 'lang site))
				(list 'ttl nil "1440"))
			(nreverse xs))))
