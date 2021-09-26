;; -*- lexical-binding: t -*-
(defun whitespacep(x) (member x '(9 10 32)))
(defun pipe(x &rest fs) (seq-reduce (lambda (x f) (funcall f x)) fs x))
(defun arrow(&rest fs) (lambda(x) (pipe x fs)))
(defun timestamp() (format-time-string "%s"))
(defun b-then-a(a b &rest args) (lambda() (apply b args) (funcall a)))

(defun find(f xs)
	(let ((x xs) (found nil))
	(while (and x (not found))
		(when (funcall f (car x))
			(setq found (car x)))
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
