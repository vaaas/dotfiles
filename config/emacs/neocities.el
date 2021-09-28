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
		(map (lambda(x) (nc-render-item conf x))
		(seq-filter (lambda(x) (str-plist-get "skip" (nth 1 x)))
		posts)))))))
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
