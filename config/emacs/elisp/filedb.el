; -*- lexical-binding: t -*-
; functions for the quick file find db. It's a newline-separated list of directories for quickly finding files.

(defvar filedb (expand-file-name "~/filedb.txt")
	"file name for the quick find file db. This is where the results will be stored, separated by newlines")

(defvar filedb-root-dir "~/Projects"
	"directory path of where filedb-update will begin its search. Place links or symlinks under it.")

(defvar filedb-exclude-dirs
	(list
		"."
		".."
		"node_modules"
		".git"
		"public"
		"vendor"
		"build"
		"qmk_firmware"
		"qmk")
	"list of directory names that filedb-update will ignore and not traverse while building the quick find filedb")

(defun git-directory-p (x)
	"t if directory X contains a git repositor"
	(file-directory-p (concat x "/.git")))

(defun git-ls-tree (x)
	"returns a list of files tracked by a git repository in directory X"
	(with-temp-dir x
		(-> "git ls-tree -r --name-only HEAD"
		(shell-command-to-string $)
		(string-trim $)
		(split-string $ "\n")
		(mapcar (L y (concat x "/" y)) $))))

(defun filedb-walk (root disallowed f)
	(ignore-errors
	(-> root
	(directory-files $)
	(seq-filter (outside disallowed) $)
	(mapcar (L x (concat root "/" x)) $)
	(C seq-each $ (L x (cond
		((file-directory-p x)
			(if (git-directory-p x)
				(dolist (name (git-ls-tree x))
					(funcall f name))
				(filedb-walk x disallowed f)))
		(t (funcall f x))))))
	nil))

(defun filedb-update ()
	"Update the filedb file. The name of the filedb file is determined in the `filedb' variable.
Begin walking from `filedb-root-dir' and exclude directories in `filedb-exclude-dirs'."
	(interactive)
	(message "Updating filedb. This may take a while...")
	(let ((skip (+ 1 (length filedb-root-dir))))
	(with-temp-file filedb
		(filedb-walk filedb-root-dir filedb-exclude-dirs
			(L x (insert (substring x skip (length x)) "\n")))))
	(message "Done."))

(defun filedb-find-file ()
	"find-file by searching the filedb file. filedb is a newline-separated list of files.
Update the filedb through `filedb-update' periodically."
	(interactive)
	(-> (slurp filedb)
	(split-string $ "\n")
	(ido-completing-read "select file: " $)
	(concat filedb-root-dir "/" $)
	(find-file $)))
