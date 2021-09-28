;; -*- lexical-binding: t -*-
(defun nc-render() (interactive)
	(let
		((conf ())
		(site (read-sexp-from-file (concat blog-directory "/site.sexp")))
		(posts ())
		(pages ()))
	(dolist (x site) (cond
		((member (car x) '("icon" "sitename" "lang" "url" "author" "links"))
			(push (cadr x) conf)
			(push (car x) conf))
		((string= (car x) "links")
			(push (cdr x) conf)
			(push (car x) conf))
		((string= (car x) "posts")
			(setq posts (mapcar 'vasdown-to-seml (cdr x))))
		((string= (car x) "pages")
			(setq pages (mapcar 'vasdown-to-seml (cdr x))))))
	(with-temp-file (concat blog-directory "/render/index.html")
		(concat "<!DOCTYPE html>" (seml-to-html
		(nc-inline
		(nc-frontpage conf
		(map 'nc-render-item
		(seq-filter (lambda(x) (str-plist-get "skip" (nth 1 x))))
		posts))))))
	(with-temp-file (concat blog-directory "/render/rss.xml")
		(concat "<?xml version='1.0' encoding='UTF-8'?>" (seml-to-html
		(nc-rss conf
		(map (lambda(x) (nc-rss-item conf x))
		(seq-filter (lambda(x) (str-plist-get "skip" (nth 1 x)))
		posts))))))
	(dolist (x (seq-filter (lambda(x) (str-plist-get "filename" (nth 1 x))) posts))
		(with-temp-file (concat blog-directory "/render/" (str-plist-get "filename" x))
			(concat "<!DOCTYPE html>" (seml-to-html (nc-inline (nc-post x))))))
	(dolist (x (seq-filter (lambda(x) (str-plist-get "filename" (nth 1 x))) posts))
		(with-temp-file (concat blog-directory "/render/" (str-plist-get "filename" x))
			(concat "<!DOCTYPE html>" (seml-to-html (nc-inline (nc-page x))))))))

(defun nc-rencer-item (x)
	(list "article"
		(list ("id" (nc-guid (string-to-number (str-plist-get "timestamp" (nth 1 x)))))
			("t" (str-plist-get "tag" (nth 1 x))))
		(nc-description x)))

(defun nc-description (x)
	(let ((xs ()) (h1 nil) (p nil))
	(push (list "time" nil (ymd (int (str-plist-get "timestamp" (nth 1 x))))) x)
	(if (str-plist-get "filename" x)
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
		(push (nth 2 p)))
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
