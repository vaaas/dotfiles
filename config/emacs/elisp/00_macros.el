; -*- lexical-binding: t -*-
; various utility macros

(defmacro L (x &rest body)
	"lambda shorthand for only one argument."
	`(lambda (,x) ,@body))

(defmacro LL (&rest xs)
	"lambda shorthand for partial functions."
	(let ((x (gensym)))
	`(lambda (,x) (,@xs ,x))))

(defmacro C (f &rest xs)
	"call function F with the last 2 arguments XS in reverse."
	(let ((head (butlast xs 2)) (tail (last xs 2)))
	(append (list f) head (nreverse tail))))

(defmacro -> (&rest body)
	"Pipeline macro. Pass the first element of body to the second, replacing the placeholder `$'.
Thus, (+ 1 (+ 2 x)) becomes (-> x (+ 2 $) (+ 1 $))"
	(let ((result (pop body)))
	(dolist (form body result)
		(setq result (mapcar (L x (if (eq '$ x) result x)) form)))))

(defmacro => (&rest body)
	"Like ->, excepts returns a lambda, that when evaluated, will run its argument through the pipeline.
Thus, (mapcar (lambda (x) (+ 1 (+ 2 x))) xs) becomes (mapcar (=> (+ 2 $) (+ 1 $)) xs)"
	`(L x (-> x ,@body)))

(defmacro defun* (name args comment &rest body)
	"Alternate defun implementation for curried functions."
	(defun arg-help (x) (if (listp x) x (list x)))
	(defun nest (args)
		(if (cdr args)
			`(lambda ,(arg-help (car args)) ,(nest (cdr args)))
			`(lambda ,(arg-help (car args)) ,@body)))
	`(defun ,name ,(arg-help (car args)) ,comment ,(nest (cdr args))))

(defmacro thrush (x &rest fs)
	(dolist (f fs x)
		(setq x (if (symbolp f) (list f x) (append f (list x))))))

(defmacro thrush* (&rest fs)
	(let* ((x (gensym)) (r x))
	`(L ,x ,(macroexpand `(thrush ,x ,@fs)))))

(defmacro ignore-errors (&rest body)
	"Execute BODY, returning nil on errors."
	`(condition-case nil (progn ,@body) (error nil)))

(defmacro with-temp-dir (temp-dir &rest body)
	"Change directory to TEMP-DIR. Execute BODY. Then return to the previous directory."
	`(let ((old default-directory))
	(cd ,temp-dir)
	(unwind-protect (progn ,@body) (cd old))))

(defmacro push-all (from to)
	"`push' all elements of FROM onto TO"
	`(dolist (x ,from ,to) (setq ,to (cons x ,to))))

(defmacro with-contents-function (buffer setup &rest rest)
	"Switch to buffer BUFFER, optionally running SETUP. Set after-save and before-save hooks to nil, then set write-contents-functions to a single function, as defined in REST.

This is useful for creating temporary non-file buffers and waiting for the user to save the buffer to continue execution."
	`(progn
		(switch-to-buffer ,buffer)
		(erase-buffer)
		,setup
		(setq-local
			after-save-hook nil
			before-save-hook nil
			write-contents-functions (list (lambda nil ,@rest (kill-buffer ,buffer))))
		(concat "Editing virtual " ,buffer ". File will not be saved.")))

(defmacro edit-buffer-region (buffer start end &rest setup)
	"Create a buffer BUFFER, cloning the region from START to END, optionally running SETUP. Upon save, replaces the original buffer's region with the contents of BUFFER."
	`(let ((prev (current-buffer)))
	(with-contents-function ,buffer
		(progn
			(insert-buffer-substring-no-properties prev ,start ,end)
			(beginning-of-buffer)
			,@setup)
		(switch-to-buffer prev)
		(delete-region ,start ,end)
		(goto-char ,start)
		(insert-buffer-substring-no-properties ,buffer)
		(goto-char ,start))))
