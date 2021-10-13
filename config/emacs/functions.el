; -*- lexical-binding: t -*-
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

(defmacro push-all (xs list)
	`(dolist (x ,xs) (setq ,list (cons x ,list))))

(defmacro with-contents-function (buffer setup &rest rest)
	`(progn
		(switch-to-buffer ,buffer)
		(erase-buffer)
		,setup
		(setq-local
			after-save-hook nil
			before-save-hook nil
			write-contents-functions (list (lambda() ,@rest)))
		(concat "Editing virtual " ,buffer ". File will not be saved.")))

(defun read-elisp-file (file)
	(with-current-buffer (find-file-noselect file)
	(goto-char (point-min))
	(read (current-buffer))))

(defun read-xml-file (file)
	(with-current-buffer (find-file-noselect file)
	(libxml-parse-xml-region (point-min) (point-max))))

(defun int-to-base (n base)
	(if (= n 0) "0"
	(let ((xs nil)
		(neg (< n 0))
		(x (abs n))
		(digits "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-_"))
	(while (> x 0)
		(push (elt digits (% x base)) xs)
		(setq x (/ x base)))
	(let ((res (string-join (mapcar #'char-to-string xs))))
	(if neg (concat "-" res) res)))))

(defun head (n x) (butlast x (- (length x) n)))
