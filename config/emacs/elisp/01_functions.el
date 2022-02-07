; -*- lexical-binding: t -*-
; various utility functions

(defun* outside (xs x)
	"Curried function. Returns lambda which checks whether X is a member of XS"
	(not (member x xs)))

(defun whitespacep (x)
	"test if a character is whitespace. Whitespace is defined as tab, space, and newline."
	(member x '(9 10 32)))

(defun timestamp nil
	"return the current unix timestamp"
	(string-to-number (format-time-string "%s")))

(defun plist-to-alist (x)
	"turn a plist into an alist"
	(if x
		(cons
			(cons (car x) (cadr x))
			(plist-to-alist (cddr x)))
		nil))

(defun alist-to-plist (x)
	(if x
		(let ((head (car x)) (tail (cdr x)))
			(cons (car head) (cons (cdr head) (alist-to-plist tail))))
		nil))

(defun alist (&rest xs)
	"create an alist. the length of XS must be even. the odd members of XS become the cars, and the even members become the cdrs."
	(plist-to-alist xs))

(defun find (f x)
	"search through XS for the element that, when passed to the callback function F, returns non-nil"
	(if x
		(let ((head (car x)) (tail (cdr x)))
			(if (funcall f head) head (find f tail)))
		nil))

(defun map-alist (f xs)
	"map for alists. Pass two arguments to F, where the first is the key, and the second is the value."
	(mapcar (L x (funcall f (car x) (cdr x))) xs))

(defun slurp (f)
	"read the contents of filepath F into a string."
	(with-temp-buffer
		(insert-file-contents f)
		(buffer-substring-no-properties (point-min) (point-max))))

(defun intersperse (s x)
	"put S between the elements XS. (1 2 3) -> (1 s 2 s 3)"
	(when x
		(let ((head (car x)) (tail (cdr x)))
			(cons head (when tail (cons s (intersperse s tail)))))))

(defun read-elisp-file (file)
	"`read' the elisp file FILE"
	(with-current-buffer (find-file-noselect file)
	(goto-char (point-min))
	(read (current-buffer))))

(defun int-to-base (n base)
	"return the string representation of integer N in base BASE. accepts positive and negative integers, and bases up to 64."
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

(defun head (n x)
	"return the first N elements of sequence X"
	(butlast x (- (length x) n)))

(defun define-new-keymap (bindings)
	"Return a new keymap with BINDINGS. First creates a sparse keymap, then fills it.
BINDINGS should be an alist where car is a `kbd' string and cdr is a function."
	(let ((map (make-sparse-keymap)))
	(dolist (x bindings map) (define-key map (kbd (car x)) (cdr x)))))

(defun sha1-ext (x)
	"sha1 a file X using external sha1 command"
	(-> (list "sha1sum" "--" x)
		(C string-join " ")
		shell-command-to-string
		(C split-string " ")
		car))

(defun difference (as bs)
	"Return the set difference of AS - BS. (items of AS that are not on BS)"
	(seq-filter (outside bs) as))

(defun string-head (n x)
	"Get the first N characters of string X. If X is shorter than N, get the entire X."
	(substring x 0 (min n (length x))))

(defun string-tail (n x)
	"Get the last N characters of string X."
	(substring x n))

(defun slice (a b x)
	"Like `substring', but the string is X and passed as a last argument."
	(substring x a b))

(defun spread-last (x)
	"spread the last element of X onto X. Useful for XML"
	(append (butlast x) (car (last x))))

(defun apply-many (f &rest xs)
	"apply F many times for each list of arguments XS"
	(dolist (x xs) (apply f x)))
