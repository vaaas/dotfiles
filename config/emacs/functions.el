;; -*- lexical-binding: t -*-
(defun whitespacep(x) (member x '(9 10 32)))
(defun timestamp() (format-time-string "%s"))
(defun plist-to-alist (xs) (map-plist #'cons xs))
(defun alist (&rest xs) (plist-to-alist xs))
(defun iff (x good bad) (if x (funcall good x) (funcall bad x)))
(defun K (x) (lambda(&rest xs) x))

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

(defun slurp(f)
	(with-temp-buffer
		(insert-file-contents f)
		(buffer-substring-no-properties (point-min) (point-max))))

(defun intersperse (s xs)
	(let ((r ()))
	(push (car xs) r)
	(dolist (x (cdr xs)) (push s r) (push x r))
	(nreverse r)))

(defmacro with-temp-dir (temp-dir &rest body)
	`(let ((old default-directory))
	(cd ,temp-dir)
	(unwind-protect (progn ,@body) (cd old))))

(defun read-elisp-file (file)
	(with-current-buffer (find-file-noselect file)
	(goto-char (point-min))
	(read (current-buffer))))
