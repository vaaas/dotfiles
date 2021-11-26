; -*- lexical-binding: t -*-
; various utility functions

(defun outside (xs)
	"Curried function. Returns lambda which checks whether X is a member of XS"
	(L x (not (member x xs))))

(defun whitespacep(x)
	"test if a character is whitespace. Whitespace is defined as tab, space, and newline."
	(member x '(9 10 32)))

(defun timestamp()
	"return the current unix timestamp"
	(string-to-number (format-time-string "%s")))

(defun plist-to-alist (xs)
	"turn a plist into an alist"
	(map-plist #'cons xs))

(defun alist (&rest xs)
	"create an alist. the length of XS must be even. the odd members of XS become the cars, and the even members become the cdrs."
	(plist-to-alist xs))

(defun find(f xs)
	"search through XS for the element that, when passed to the callback function F, returns non-nil"
	(let ((x xs) (found nil))
	(while (and x (not found))
		(when (funcall f (car x)) (setq found (car x)))
		(setq x (cdr x)))
	found))

(defun map-plist(f xs)
	"map for plists. Pass two arguments to F, where the first is the key, and the second is the value."
	(let ((head xs) (results nil))
	(while head
		(push (funcall f (car head) (cadr head)) results)
		(setq head (cddr head)))
	(nreverse results)))

(defun map-alist (f xs)
	"map for alists. Pass two arguments to F, where the first is the key, and the second is the value."
	(let ((results nil))
	(nreverse
	(dolist (x xs results)
		(push (funcall f (car x) (cdr x)) results)))))

(defun map-ip (f xs)
	"mapcar in place. modifies XS by mapping every car through F."
	(let ((head xs))
	(while head
		(setcar head (funcall f (car head)))
		(setq head (cdr head)))
	xs))

(defun map-alist-ip (f xs)
	"map-ip for alists. Pass two arguments to F, where the first is the key, and the second is the value."
	(let ((head xs))
	(while head
		(setcar head (funcall f (car (car head)) (cdr (car head))))
		(setq head (cdr head)))
	xs))

(defun slurp(f)
	"read the contents of filepath F into a string."
	(with-temp-buffer
		(insert-file-contents f)
		(buffer-substring-no-properties (point-min) (point-max))))

(defun intersperse (s xs)
	"put S between the elements XS. (1 2 3) -> (1 s 2 s 3)"
	(let ((r (cons (car xs) nil)))
	(nreverse (dolist (x (cdr xs) r) (push s r) (push x r)))))

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
	(string-join $ " ")
	(shell-command-to-string $)
	(split-string $ " ")
	(car $)))

(defun difference (as bs)
	"Return the set difference of AS - BS. (items of AS that are not on BS)"
	(seq-filter (outside bs) as))

(defun string-head (n x)
	"Get the first N characters of string X. If X is shorter than N, get the entire X."
	(substring x 0 (min n (length x))))

(defun spread-last (x)
	"spread the last element of X onto X. Useful for XML"
	(append (butlast x) (lastcar x)))
