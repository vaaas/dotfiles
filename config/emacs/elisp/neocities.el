; -*- lexical-binding: t -*-
; blog generation and uploading for neocities

(defvar nc-blog-directory (expand-file-name "~/Projects/website")
	"The directory where the neocities tools will look for the website. site.el and render/ are expected to be ther.")

(defvar nc-blog-categories '("tech" "anime" "books" "memes" "films" "journal" "games")
	"List of strings that the neocities post creation and update functions will present to the user to categorise their post.")

; general utility functions
(defun nc-guid (x)
	"Generates a unique id for a neocities article or rss item."
	(-> x (- $ 1483228800) (/ $ 60) (int-to-base $ 64))

(defun nc-ymd (x)
	"formats a timestamp in Y m d format"
	(format-time-string "%Y-%m-%d" (seconds-to-time x)))

(defun nc-rfctime (x)
	"formats a timestamp in RFC format for RSS."
	(format-time-string "%a, %d %b %Y %H:%M:%S %z" (seconds-to-time x)))

; neocities API functions
(defun nc-api-list (user password)
	"Calls the neocities list api (file listing)"
	(let ((result
		(-> (format "https://%s:%s@neocities.org/api/list" user password)
		(list "/usr/bin/curl" "-s" "--" $)
		(string-join $ " ")
		(shell-command-to-string $)
		(json-parse-string $ :object-type 'alist))))
	(if (not (string= "success" (alist-get 'result result)))
		(throw 'unsuccessful "fetching files was unsuccessful")
		(alist-get 'files result))))

(defun nc-api-delete (user password files)
	"Calls the neocities delete api"
	(message "Deleting remote neocities files...")
	(-> files
	(mapcar (=> (format "-d filenames[]=%s" $)) $)
	(string-join $ " ")
	(format "/usr/bin/curl %s https://%s:%s@neocities.org/api/delete" $)
	(shell-command-to-string $)))

(defun nc-api-upload (user password files)
	"Calls the neocities upload api"
	(message "Uploading local files to neocities. This may take a while...")
	(with-temp-dir (concat nc-blog-directory "/render")
		(-> files
		(mapcar (=> (format "-F %s=@%s" $ $)) $)
		(string-join $ " ")
		(format "/usr/bin/curl %s https://%s:%s@neocities.org/api/upload" $ user password)
		(shell-command-to-string $))))

(defun nc-push()
	"Push local files to neocities."
	(interactive)
	(let*
		((user (->
			(concat nc-blog-directory "/site.el")
			(read-elisp-file $)
			(alist-get 'conf $)
			(alist-get 'username $)))
		(password (read-passwd (concat "Password for " user ": ")))
		(remote-files
			(-> (nc-api-list user password)
			(seq-filter (L x (eq :false (alist-get 'is_directory x))) $)
			(mapcar (L x (cons (alist-get 'path x) (alist-get 'sha1_hash x))) $)))
		(local-files
			(-> (concat nc-blog-directory "/render")
			(directory-files-recursively $ ".*")
			(mapcar (L x (cons (substring x (length (concat nc-blog-directory "/render/")) (length x)) (sha1-ext x))) $)))
		(delete-these-files (difference (mapcar #'car remote-files) (mapcar #'car local-files)))
		(upload-these-files (mapcar #'car (difference local-files remote-files))))
	(if delete-these-files
		(whenl reply (yes-or-no-p (concat "Deleting " (string-join delete-these-files " ") ": "))
			(nc-api-delete user password delete-these-files)
			(message "Done"))
		(message "Nothing to delete"))
	(if upload-these-files
		(whenl reply (yes-or-no-p (concat "Uploading " (string-join upload-these-files " ") ": "))
			(nc-api-upload user password upload-these-files)
			(message "Done"))
		(message "Nothing to upload"))))

; neocities render functions
(defun nc-render-html (lang head body)
	"Generates an HTML element."
	(list 'html (alist 'lang lang) head body))

(defun nc-render-head (conf title)
	"Generates the HEAD element of an html file."
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

(defun nc-render()
	"Render the site defined in variable `nc-blog-directory' for neocities. Expects file site.el to be present in the blog directory, and also the directory render."
	(interactive)
	(let*
		((site (read-elisp-file (concat nc-blog-directory "/site.el")))
		(conf (alist-get 'conf site))
		(posts (alist-get 'posts site))
		(pages (alist-get 'pages site))
		(doctype "<!DOCTYPE html>"))

	; render index.html
	(with-temp-file (concat nc-blog-directory "/render/index.html")
		(-> posts
		(seq-filter (=> (nth 1 $) (alist-get 'skip $) (not $)) $)
		(mapcar #'nc-render-item $)
		(nc-render-frontpage conf $)
		(serialise-xml $)
		(concat doctype $)
		(insert $)))

	; render rss.xml
	(with-temp-file (concat nc-blog-directory "/render/rss.xml")
		(-> posts
		(seq-filter (=> (nth 1 $) (alist-get 'skip $) (not $)) $)
		(mapcar (=> (nc-render-rss-item conf $)) $)
		(nc-render-rss conf $)
		(serialise-xml $)
		(concat "<?xml version='1.0' encoding='UTF-8'?>" $)
		(insert $)))

	; render individual articles and pages
	(dolist (pair (alist posts #'nc-render-post pages #'nc-render-page))
		(dolist (x (car pair))
			(whenl filename (alist-get 'filename x)
			(with-temp-file (concat nc-blog-directory "/render/" filename)
				(-> x
					(funcall (cdr pair) conf $)
					(serialise-xml $)
					(concat doctype $)
					(insert $))))))))

(defun nc-render-item (x)
	"Render an article element."
	(spread-last (list
		'article
		(alist
			't (-> x (nth 1 $) (alist-get 'tag $))
			'id (-> x (nth 1 $) (alist-get 'timestamp $) (nc-guid $)))
		(nc-render-description x))))

(defun nc-render-description (x)
	"Generate the description of an article or RSS item."
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
			(list 'h1 nil (spread-last (list 'a (alist 'href (concat "/" filename)) (cddr h1))))
			xs)
		(push-all (cddr p) xs)))
	(nreverse xs)))

(defun nc-render-frontpage (conf xs)
	"Generate the frontpage (index.html) of the neocities blog."
	(let
	((distinct-tags
		(-> xs
		(mapcar (=> (nth 1 $) (alist-get 't $)) $)
		(delete-dups $)
		(sort $ #'string<))))
	(nc-render-html
		(alist-get 'lang conf)
		(nc-render-head conf (alist-get 'sitename conf))
		(list 'body nil
			(list 'header nil
				(list 'div (alist 'class "imgtxt")
					(list 'img (alist 'src "/pics/banner.jpg"))
					(list 'h1 nil (alist-get 'sitename conf)))
				(spread-last (list 'p nil (alist-get 'blurb conf))))
			(append
				(list 'nav nil)
				(alist-get 'links conf)
				(list (list 'a (alist 'class "active" 'href "all") "all"))
				(mapcar (L x (list 'a (alist 'href x) x)) distinct-tags))
			(spread-last (list 'main nil (nreverse xs)))
			(list 'script (alist 'src "/script.js") " ")))))

(defun nc-render-rss-item (conf x)
	"Creates an rss item."
	(let*
		((attrs (nth 1 x))
		(conf-url (alist-get 'url conf))
		(timestamp (alist-get 'timestamp attrs))
		(filename (alist-get 'filename attrs))
		(guid (nc-guid timestamp))
		(url (if filename
			(concat conf-url "/" filename)
			(concat conf-url "/#" guid))))
	(list 'item nil
		(list 'title nil
			(if filename
				(-> x (query-selector (xml-elem= 'h1) $) (xml-inner-text $))
				(format "New post by %s (%s)" (alist-get 'author conf) guid)))
		(list 'guid nil url)
		(list 'pubDate nil (nc-rfctime timestamp))
		(list 'link nil url)
		(list 'description nil
			(spread-last (list '!cdata nil
			(->(nc-render-description x)
			(cdr $)
			(mapcar (=> (nc-render-absolute-links conf-url $)) $))))))))

(defun nc-render-rss (conf xs)
	"Creates rss.xml."
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

(defun nc-render-post (conf x)
	"Generate an individual neocities article."
	(let ((h1 (query-selector (xml-elem= 'h1) x))
		(timestamp (alist-get 'timestamp (nth 1 x))))
	(when (not h1) (throw 'bad-post "No h1 found"))
	(nc-render-html
		(alist-get 'lang conf)
		(nc-render-head conf (xml-inner-text h1))
		(list 'body (alist 'class "post")
			(list 'header nil
				(list 'a (alist 'href "/") (alist-get 'sitename conf))
				" — "
				(list 'time nil (nc-ymd timestamp)))
			(spread-last (list 'main nil (cddr x)))))))

(defun nc-render-page (conf x)
	"Generate an individual neocities page"
	(let
		((head (query-selector (xml-elem= 'head) x))
		(body (query-selector (xml-elem= 'body) x)))
	(nc-render-html
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
		(spread-last (list 'body (alist 'class "page") (cddr body))))))

; neocities CRUD functions
(defun nc-make-post nil
	"Create a neocities blog article."
	(interactive)
	(let*
		((buffer (current-buffer))
		(site-file (concat nc-blog-directory "/site.el"))
		(cat (ido-completing-read "category?> " nc-blog-categories))
		(stamp (timestamp))
		(file-name
			(when (>= (buffer-size) 2000)
				(-> (read-string "file name (no extension): ")
				(replace-regexp-in-string " " "_" $)
				(concat $ ".html"))))
		(post (spread-last
			(list 'post
			(if file-name
				(alist 'timestamp stamp 'tag cat 'filename file-name)
				(alist 'timestamp stamp 'tag cat))
			(with-temp-buffer
				(insert-buffer buffer)
				(cmark)
				(beginning-of-buffer) (insert "<body>")
				(end-of-buffer) (insert "</body>")
				(cddr (libxml-parse-xml-region (point-min) (point-max)))))))
		(site (read-elisp-file site-file))
		(posts (alist-get 'posts site)))
	(with-temp-file (concat nc-blog-directory "/site.el")
		(setcdr (last posts) (cons post nil))
		(pp site (current-buffer)))))

(defun nc-post-preview (x)
	"Generates a string preview for a post for autocompletion."
	(let ((h1 (query-selector (xml-elem= 'h1) x)))
	(concat
		(number-to-string (alist-get 'timestamp (nth 1 x)))
		" "
		(if h1
			(xml-inner-text h1)
			(string-head 128 (xml-inner-text x))))))

(defun nc-edit-post nil
	"Edit a neocities blog post."
	(interactive)
	(let*
		(site (read-elisp-file (concat nc-blog-directory "/site.el"))
		(posts (-> site
			(alist-get 'posts $)
			(C mapcar $ (L x (cons (alist-get 'timestamp (nth 1 x)) x))))
		(selected-post
			(-> posts
			(C mapcar $ (=> (cadr $) (nc-post-preview $)))
			(ido-completing-read "select post: " $)
			(split-string $ " ")
			(car $)
			(string-to-number $)
			(alist-get $ posts)))))
	(with-contents-function "*edit-blog-post*"
	(progn
		(insert (serialise-xml selected-post))
		(xml-mode))
	(let ((edited-post
		(having x (libxml-parse-xml-region (point-min) (point-max))
		(-> x
		(nth 1 $)
		(C map-alist-ip $
			(lambda (k v)
				(cons k (cond
					((eq k 'skip) (intern v))
					((eq k 'timestamp) (string-to-number v))
					(t v)))))))))
	(setf
		(nth 1 selected-post) (nth 1 edited-post)
		(cddr selected-post) (cddr edited-post))
	(print site)))))

(defun nc-render-absolute-links (prefix x)
	"turn all links in the href and src properties of all elements in the dom tree X into absolute links by prefixing them with PREFIX."
	(if (listp x)
	(spread-last (list
		(nth 0 x)
		(C map-alist (nth 1 x)
			(lambda (k v) (cons k
				(if (and (member k '(href src)) (string-prefix-p "/" v))
				(concat prefix v)
				v))))
		(C mapcar (cddr x) (=> (nc-render-absolute-links prefix $)))))
	x))
