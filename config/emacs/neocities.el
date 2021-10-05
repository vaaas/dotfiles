;; -*- lexical-binding: t -*-
(defun nc-render() (interactive)
	(let
		((conf ())
		(site (car (read-sexp-from-file (concat blog-directory "/site.lisp"))))
		(posts ())
		(pages ()))

	; load data frome site file
	(dolist (x (cdr site))
		(cond
		((member (car x) '("icon" "sitename" "lang" "url" "author"))
			(push (cadr x) conf)
			(push (car x) conf))
		((string= (car x) "links")
			(push (cdr x) conf)
			(push (car x) conf))
		((string= (car x) "posts")
			(setq posts (mapcar 'vasdown-to-seml (cdr x))))
		((string= (car x) "pages")
			(setq pages (mapcar 'vasdown-to-seml (cdr x))))))
	; render index.html
	(with-temp-file (concat blog-directory "/render/index.html")
		(insert
		(concat "<!DOCTYPE html>"
		(seml-to-html
		(nc-inline
		(nc-frontpage conf
		(mapcar 'nc-render-item
		(seq-filter (lambda(x) (not (str-plist-get "skip" (nth 1 x))))
		posts))))))))
))

	; render rss.xml
	;; (with-temp-file (concat blog-directory "/render/rss.xml")
	;; 	(concat "<?xml version='1.0' encoding='UTF-8'?>" (seml-to-html
	;; 	(nc-rss conf
	;; 	(map (lambda(x) (nc-rss-item conf x))
	;; 	(seq-filter (lambda(x) (str-plist-get "skip" (nth 1 x)))
	;; 	posts))))))

	; render individual articles
	;; (dolist (x (seq-filter (lambda(x) (str-plist-get "filename" (nth 1 x))) posts))
	;; 	(with-temp-file (concat blog-directory "/render/" (str-plist-get "filename" x))
	;; 		(concat "<!DOCTYPE html>" (seml-to-html (nc-inline (nc-post x))))))

	;; ; render individual pages
	;; (dolist (x (seq-filter (lambda(x) (str-plist-get "filename" (nth 1 x))) posts))
	;; 	(with-temp-file (concat blog-directory "/render/" (str-plist-get "filename" x))
	;; 		(concat "<!DOCTYPE html>" (seml-to-html (nc-inline (nc-page x))))))))

(defun nc-rencer-item (x)
	(list "article"
		(list ("id" (nc-guid (string-to-number (str-plist-get "timestamp" (nth 1 x)))))
			("t" (str-plist-get "tag" (nth 1 x))))
		(nc-description x)))

(defun nc-description (x)
	(let ((xs ()) (h1 ()) (p ()))
	(push (list "time" nil (nc-ymd (string-to-number (str-plist-get "timestamp" (nth 1 x))))) xs)
	(if (str-plist-get "filename" (nth 1 x))
	(progn
		(setq h1 (query-selector (seml-elem= "h1") x))
		(when (not h1) (throw 'bad-post "No h1 found"))
		(setq p (query-selector (seml-elem= "p") p))
		(when (not p) (throw 'bad-post "No p found"))
		(push (list "h1" nil (list
			(list "a"
				(list "href" (concat "/" (str-plist-get "filename" (nth 1 x))))
				(nth 2 h1))))
			xs)
		(push (nth 2 p) xs))
	(dolist (y (nth 2 x)) (push y xs)))
	(nreverse xs)))

(defun seml-elem= (s) (lambda (x) (string= s (car x))))

(defun query-selector (f node)
	(let ((head node) (found nil) (children (nth 2 node)))
	(while (and head (not found))
		(when (funcall f (car head)) (setq found (car head)))
		(while (and children (not found))
			(setq found (query-selector f (car children)))
			(setq children (cdr children)))
		(setq head (car node)))
	found))

(defun query-selector-all (f node)
	(let ((head node) (found ()) (children (nth 2 node)))
	(while head
		(when (funcall f (car head)) (push (car head) found))
		(while children
			(setcdr (last found) (query-selector f (car children)))
			(setq children (cdr children)))
		(setq head (car node)))
	found))

(defun nc-inline (x) x)

(defun nc-frontpage (conf xs)
	(let
	((distinct-tags
		(sort
		(delete-dups
		(mapcar
			(lambda (x) (str-plist-get "t" (nth 1 x)))
			xs))
		'string<)))
	(nc-html
		(str-plist-get "lang" conf)
		(nc-head conf (str-plist-get "sitename" conf))
		(list "body" nil (list
			(list "header" nil (list
				(list "div" (list "class" "imgtxt") (list
					(list "img" (list "src" "/pics/banner.jpg") nil)
					(list "h1" nil (list (str-plist-get "sitename" conf)))))))
			(list "nav" nil (append
				(str-plist-get "links" conf)
				(list (list "a" (list "class" "active" "href" "all") (list "all")))
				(mapcar (lambda (x) (list "a" (list "href" x) (list x))) distinct-tags)))
			(list "main" nil (nreverse xs))
			(list "script" (list "src" "/script.js") (list " ")))))))

(defun nc-head (conf title)
	(list "head" nil (list
		(list "meta" (list "charset" "utf8") nil)
		(list "meta" (list "name" "viewport" "content" "width=device-width, initial-scale=1.0") nil)
		(list "meta" (list "name" "url" "content" (str-plist-get "url" conf)) nil)
		(list "meta" (list "name" "author" "content" (str-plist-get "author" conf)) nil)
		(list "meta" (list "name" "description" "content" (str-plist-get "sitename" conf)) nil)
		(list "link" (list "rel" "stylesheet" "href" "/style.css") nil)
		(list "link" (list "rel" "icon" "href" "/favicon.ico") nil)
		(list "link" (list "rel" "alternate" "href" "/rss.xml" "type" "application/rss+xml") nil)
		(list "title" nil (list title)))))

(defun nc-html(lang head body)
	(list "html" (list "lang" lang) (list head body)))

(defun nc-render-item (x)
	(list "article" (list
		"id" (nc-guid (string-to-number (str-plist-get "timestamp" (nth 1 x))))
		"t" (str-plist-get "tag" (nth 1 x)))
		(nc-description x)))

(defun nc-guid (x) "43892174312")
(defun nc-ymd (x) "6666-69-42")
