; -*- lexical-binding: t -*-
; various utility functions

(defun outside (xs)
	"Curried function. Returns lambda which checks whether X is a member of XS"
	(lambda (x) (not (member x xs))))

(defun whitespacep(x)
	"test if a character is whitespace. Whitespace is defined as tab, space, and newline."
	(member x '(9 10 32)))

(defun timestamp()
	"return the current unix timestamp"
	(format-time-string "%s"))

(defun plist-to-alist (xs)
	"turn a plist into an alist"
	(map-plist #'cons xs))

(defun alist (&rest xs)
	"create an alist. the length of XS must be even. the odd members of XS become the cars, and the even members become the cdrs."
	(plist-to-alist xs))

(defun iff (x good bad)
	"execute GOOD with argument X if X is non-nil. Otherwise, execute BAD with argument X"
	(if x (funcall good x) (funcall bad x)))

(defun K (x)
	"return a function that, when executed, always returns X"
	(lambda(&rest xs) x))

(defun find(f xs)
	"search through XS for the element that, when passed to the callback function F, returns non-nil"
	(let ((x xs) (found nil))
	(while (and x (not found))
		(when (funcall f (car x)) (setq found (car x)))
		(setq x (cdr x)))
	found))

(defun map-plist(f xs)
	"map for plists. Pass two arguments to F, where the first is the key, and the second is the value."
	(let ((head xs) (results ()))
	(while head
		(push (funcall f (car head) (cadr head)) results)
		(setq head (cddr head)))
	(nreverse results)))

(defun slurp(f)
	"read the contents of filepath F into a string."
	(with-temp-buffer
		(insert-file-contents f)
		(buffer-substring-no-properties (point-min) (point-max))))

(defun intersperse (s xs)
	"put S between the elements XS. (1 2 3) -> (1 s 2 s 3)"
	(let ((r nil))
	(push (car xs) r)
	(dolist (x (cdr xs)) (push s r) (push x r))
	(nreverse r)))

(defmacro with-temp-dir (temp-dir &rest body)
	"Change directory to TEMP-DIR. Execute BODY. Then return to the previous directory."
	`(let ((old default-directory))
	(cd ,temp-dir)
	(unwind-protect (progn ,@body) (cd old))))

(defmacro push-all (xs list)
	"`push' all elements of LIST to XS"
	`(dolist (x ,xs) (setq ,list (cons x ,list))))

(defun read-elisp-file (file)
	"`read' the elisp file FILE"
	(with-current-buffer (find-file-noselect file)
	(goto-char (point-min))
	(read (current-buffer))))

(defun read-xml-file (file)
	"parse the the XML file FILE. see `libxml-parse-xml-region'"
	(with-current-buffer (find-file-noselect file)
	(libxml-parse-xml-region (point-min) (point-max))))

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
	(dolist (x bindings)
		(define-key map (kbd (car x)) (cdr x)))
	map))

(defun sha1-ext (x)
	"sha1 a file X using external sha1 command"
	(car (split-string (shell-command-to-string (string-join (list "sha1sum" "--" x) " ")) " ")))

(defun difference (as bs)
	"Return the set difference of AS - BS. (items of AS that are not on BS)"
	(seq-filter (outside bs) as))

(defmacro with-contents-function (buffer setup &rest rest)
	"Switch to buffer BUFFER, optionally running SETUP. Set after-save and before-save hooks to nill, then set write-contents-functions to a single function, as defined in REST.

This is useful for creating temporary non-file buffers and waiting for the user to save the buffer to continue execution."
	`(progn
		(switch-to-buffer ,buffer)
		(erase-buffer)
		,setup
		(setq-local
			after-save-hook nil
			before-save-hook nil
			write-contents-functions (list (lambda() ,@rest (kill-buffer ,buffer))))
		(concat "Editing virtual " ,buffer ". File will not be saved.")))

(defvar narrow-indirect-mode-map (define-new-keymap (alist "C-c C-c" (lambda() (interactive) (kill-this-buffer)))))
(define-minor-mode narrow-indirect-mode "A narrowed, indirect buffer." :keymap 'narrow-indirect-mode-map)
(defmacro narrow-indirect (buffer start end &rest setup)
	`(progn
		(make-indirect-buffer (current-buffer) ,buffer)
		(switch-to-buffer ,buffer)
		(narrow-to-region ,start ,end)
		,@setup
		(narrow-indirect-mode 1)))

(defmacro edit-buffer-region (buffer start end &rest setup)
	`(let ((prev (current-buffer)))
		(kill-ring-save ,start ,end)
		(with-contents-function ,buffer (progn (yank) (beginning-of-buffer) ,@setup)
			(kill-ring-save (point-min) (point-max))
			(switch-to-buffer prev)
			(delete-region ,start ,end)
			(goto-char ,start)
			(yank))))
