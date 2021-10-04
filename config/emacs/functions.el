;; -*- lexical-binding: t -*-
(defun whitespacep(x) (member x '(9 10 32)))
(defun timestamp() (format-time-string "%s"))

(defun find(f xs)
	(let ((x xs) (found nil))
	(while (and x (not found))
		(when (funcall f (car x)) (setq found (car x)))
		(setq x (cdr x)))
	found))

(defun map-plist(f xs)
	(let ((head xs) (results ()))
		(while head
			(push (funcall f (car head) (cadr head)) results)
			(setq head (cddr head)))
		(nreverse results)))

(defun str-plist-get(k xs)
	(let ((x xs) (found nil))
	(while (and x (not found))
		(when (string= (car x) k) (setq found (cadr x)))
		(setq x (cddr x)))
	found))

(defun slurp(f)
	(with-temp-buffer
		(insert-file-contents f)
		(buffer-substring-no-properties (point-min) (point-max))))
