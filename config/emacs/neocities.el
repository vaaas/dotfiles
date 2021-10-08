;; -*- lexical-binding: t -*-
(defun nc-render() (interactive)
	(let
		((site (read-elisp-file (concat blog-directory "/site.lisp")))
		(conf nil)
		(posts nil))
	; load data frome site file
	(setq conf (alist
		'icon (alist-get 'icon site)
		'sitename (alist-get 'sitename site)
		'lang (alist-get 'lang site)
		'url (alist-get 'url site)
		'author (alist-get 'author site)
		'links (alist-get 'links site)))
	(setq posts (alist-get 'posts site))
	; render index.html
	(with-temp-file (concat blog-directory "/render/index.html")
		(insert
		(concat "<!DOCTYPE html>"
		(serialise-xml
		(nc-frontpage conf
		(mapcar #'nc-render-item
		(seq-filter (lambda (x) (not (alist-get "skip" (nth 1 x))))
		posts)))))))
	; render rss.xml
	(with-temp-file (concat blog-directory "/render/rss.xml")
		(insert
		(concat "<?xml version='1.0' encoding='UTF-8'?>"
		(nc-rss conf
		(mapcar #'nc-rss-item
		(seq-filter (lambda (x) (not (alist-get "skip" (nth 1 x))))
		posts))))))

))


	; render individual articles
	;; (dolist (x (seq-filter (lambda(x) (str-plist-get "filename" (nth 1 x))) posts))
	;; 	(with-temp-file (concat blog-directory "/render/" (str-plist-get "filename" x))
	;; 		(concat "<!DOCTYPE html>" (seml-to-html (nc-inline (nc-post x))))))

	;; ; render individual pages
	;; (dolist (x (seq-filter (lambda(x) (str-plist-get "filename" (nth 1 x))) posts))
	;; 	(with-temp-file (concat blog-directory "/render/" (str-plist-get "filename" x))
	;; 		(concat "<!DOCTYPE html>" (seml-to-html (nc-inline (nc-page x))))))))

(defun nc-rencer-item (x)
	(append
		(list 'article (alist
			't (alist-get 'tag (nth 1 x))
			'id (nc-guid (alist-get 'timestamp (nth 1 x)))))
		(nc-description x))

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
		(mapcar (lambda (x) (symbol-name (alist-get 't (nth 1 x)))) xs)
		#'string<))))
	(nc-html
		(alist-get 'lang conf)
		(nc-head conf (alist-get 'sitename conf))
		(list 'body nil
			(list 'header nil
				(list 'div (alist 'class "imgtxt")
					(list 'img (alist 'src "/pics/banner.jpg"))
					(list 'h1 nil (alist-get 'sitename conf))))
			(list 'nav nil (append
				(alist-get 'links conf)
				(list (list a' (alist 'class "active" 'href "all") "all"))
				(mapcar (lambda (x) (list 'a (alist 'href x) x)) distinct-tags)))
			(append (list 'main nil) (nreverse xs))
			(list 'script (list 'src "/script.js") " ")))))

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

(defun nc-html(lang head body) (list 'html (alist 'lang lang) head body)

(defun nc-render-item (x)
	(append
		(list 'article (alist 'id (nc-guid (alist-get 'timestamp (nth 1 x))) 't (alist-get 't (nth 1 x))))
		(nc-description x)))

(defun nc-guid (x) "43892174312")
(defun nc-ymd (x) "6666-69-42")
