;;; -*- lexical-binding: t -*-
(defun nc-render() (interactive)
	(let*
		((site (read-elisp-file (concat blog-directory "/site.el")))
		(conf (alist-get 'conf site))
		(posts (alist-get 'posts site))
		(pages (alist-get 'pages site))
		(doctype "<!DOCTYPE html>"))

	; render index.html
	(with-temp-file (concat blog-directory "/render/index.html")
		(insert
		(concat doctype
		(serialise-xml
		(nc-frontpage conf
		(mapcar #'nc-render-item
		(seq-filter (lambda (x) (not (alist-get 'skip (nth 1 x))))
		posts)))))))

	; render rss.xml
	(with-temp-file (concat blog-directory "/render/rss.xml")
		(insert
		(concat "<?xml version='1.0' encoding='UTF-8'?>"
		(serialise-xml
		(nc-rss conf
		(mapcar (lambda (x) (nc-rss-item conf x))
		(seq-filter (lambda (x) (not (alist-get 'skip (nth 1 x))))
		posts)))))))

	; render individual articles
	(dolist (x (seq-filter (lambda(x) (alist-get 'filename (nth 1 x))) posts))
		(with-temp-file (concat blog-directory "/render/" (alist-get 'filename (nth 1 x)))
			(insert (concat doctype (serialise-xml (nc-post conf x))))))

	; render individual pages
	(dolist (x (seq-filter (lambda(x) (alist-get 'filename (nth 1 x))) pages))
		(with-temp-file (concat blog-directory "/render/" (alist-get 'filename (nth 1 x)))
			(insert (concat doctype (serialise-xml (nc-page conf x))))))))

(defun nc-render-item (x)
	(append
		(list 'article (alist
			't (alist-get 'tag (nth 1 x))
			'id (nc-guid (alist-get 'timestamp (nth 1 x)))))
		(nc-description x)))

(defun nc-description (x)
	(let
		((filename (alist-get 'filename (nth 1 x)))
		(xs (list (list 'time nil (nc-ymd (alist-get 'timestamp (nth 1 x)))))))
	(if (not filename) (push-all (cddr x) xs)
	(let
		((h1 (query-selector (xml-elem= 'h1) x))
		(p (query-selector (xml-elem= 'p) x)))
		(when (not h1) (throw 'bad-post "No h1 found"))
		(when (not p) (throw 'bad-post "No p found"))
		(push
			(list 'h1 nil (append
				(list 'a (alist 'href (concat "/" filename)))
				(cddr h1)))
			xs)
		(push-all (cddr p) xs)))
	(nreverse xs)))

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
				(append (list 'p nil) (alist-get 'blurb conf)))
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

(defun nc-html (lang head body) (list 'html (alist 'lang lang) head body))
(defun nc-guid (x) (int-to-base (/ (- x 1483228800) 60) 64))
(defun nc-ymd (x) (format-time-string "%Y-%m-%d" (seconds-to-time x)))
(defun nc-rfctime (x) (format-time-string "%a, %d %b %Y %H:%M:%S %z" (seconds-to-time x)))

(defun nc-rss-item (conf x)
	(let*
		((timestamp (alist-get 'timestamp (nth 1 x)))
		(guid (nc-guid timestamp))
		(date (nc-rfctime timestamp))
		(url (concat (alist-get 'url conf) "/#" guid)))
	(list 'item nil
		(list 'title nil
			(iff (query-selector (xml-elem= 'h1) x)
				#'xml-inner-text
				(K (format "New post by %s (%s)" (alist-get 'author conf) guid))))
		(list 'guid nil url)
		(list 'pubDate nil date)
		(list 'link nil url)
		(append
			(list 'description nil)
			(mapcar (lambda (x) (xml-escape-string (serialise-xml x))) (nc-description x))))))

(defun nc-rss (conf xs)
	(list 'rss (alist 'version "2.0" 'xmlns:atom "http://www.w3.org/2005/Atom")
		(append
			(list 'channel nil
				(list 'title nil (alist-get 'sitename conf))
				(list 'link nil (alist-get 'url conf))
				(list 'atom:link
					(alist 'href (concat (alist-get 'url conf) "/rss.xml")
						'rel "self"
						'type "application/rss+xml"))
				(list 'description nil (alist-get 'sitename conf))
				(list 'pubDate nil (xml-inner-text (query-selector (xml-elem= 'pubDate) (car xs))))
				(list 'language nil (alist-get 'lang conf))
				(list 'ttl nil "1440"))
			(nreverse xs))))

(defun nc-post (conf x)
	(let ((h1 (query-selector (xml-elem= 'h1) x))
		(timestamp (alist-get 'timestamp (nth 1 x))))
	(when (not h1) (throw 'bad-post "No h1 found"))
	(nc-html
		(alist-get 'lang conf)
		(nc-head conf (xml-inner-text h1))
		(list 'body (alist 'class "post")
			(list 'header nil
				(list 'a (alist 'href "/") (alist-get 'sitename conf))
				" — "
				(list 'time nil (nc-ymd timestamp)))
			(append (list 'main nil) (cddr x))))))

(defun nc-page (conf x)
	(let
		((head (query-selector (xml-elem= 'head) x))
		(body (query-selector (xml-elem= 'body) x)))
	(nc-html
		(alist-get 'lang conf)
		(append
			(list 'head nil
				(list 'meta (alist 'charset "utf8"))
				(list 'meta (alist 'name "viewport" 'content "width=device-width, initial-scale=1.0"))
				(list 'meta (alist 'name "url" 'content (alist-get 'url conf)))
				(list 'meta (alist 'name "author" 'content (alist-get 'author conf)))
				(list 'meta (alist 'name "description" 'content (alist-get 'sitename conf)))
				(list 'link (alist 'rel "icon" 'href "/favicon.ico"))
				(list 'link (alist 'rel "alternate" 'href "/rss.xml" 'type "application/rss+xml")))
			(cddr head))
		(append
			(list 'body (alist 'class "page"))
			(cddr body)))))

(defun blog() (interactive)
	(let*
		((buffer (current-buffer))
		(site-file (concat blog-directory "/site.el"))
		(cat (ido-completing-read "category?> " blog-categories))
		(stamp (timestamp))
		(file-name (when (>= (buffer-size) 2000)
			(concat
				(replace-regexp-in-string " " "_"
					(read-string "file name (no extension): "))
				".html")))
		(post (append
			(list 'post (if file-name
				(alist 'timestamp stamp 'tag cat 'filename file-name)
				(alist 'timestamp stamp 'tag cat)))
			(with-temp-buffer
				(insert-buffer buffer)
				(cmark)
				(beginning-of-buffer) (insert "<body>")
				(end-of-buffer) (insert "</body>")
				(cddr (libxml-parse-xml-region (point-min) (point-max))))))
		(site (read-elisp-file site-file))
		(posts (alist-get 'posts site)))
	(with-temp-file (concat blog-directory "/site.el")
		(setcdr (last posts) (cons post nil))
		(prin1 site (current-buffer)))))

(defun edit-blog-post() (interactive)
	; TODO: finish this
	(let*
		((site (read-elisp-file (concat blog-directory "/site.el")))
		(posts (alist-get 'posts site))
		(choices (mapcar (lambda (x)
				(let ((h1 (query-selector (xml-elem= 'h1) x)))
				(concat
					(number-to-string (alist-get 'timestamp (nth 1 x)))
					" "
					(if h1 (xml-inner-text h1)
					(let ((txt (xml-inner-text x)))
						(substring txt 0 (min 128 (length txt))))))))
			posts))
		(choice (ido-completing-read "select post: " choices))
		(selected-timestamp (string-to-number (car (split-string choice " "))))
		(selected-post (find (lambda (x) (= selected-timestamp (alist-get 'timestamp (nth 1 x)))) posts)))
	(with-contents-function "*edit-blog-post*"
		(progn
			(insert (serialise-xml selected-post))
			(xml-mode))
		(print "yo"))))

;; (defun nc-push()
;; 	(let*
;; 		((conf
;; 			(alist-get 'conf
;; 			(read-elisp-file (concat blog-directory "/site.el")))))
;; 		(user (alist-get 'username conf))
;; 		(password (read-passwd (concat "Password for " url ": ")))
;; 		(url (format "https://%s:%s@neocities.org" user password))
;; 		(remote-files
;; 			(let ((response
;; 				(json-parse-string
;; 				(shell-command-to-string
;; 				(string-join "/usr/bin/curl" "--" url)))))
;; 			(when (alist-get 'result
;; )
