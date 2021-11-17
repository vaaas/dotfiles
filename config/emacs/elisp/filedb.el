; -*- lexical-binding: t -*-
; functions for the quick file find db. It's a newline-separated list of directories for quickly finding files.

(defvar filedb (expand-file-name "~/filedb.txt")
	"file name for the quick find file db. This is where the results will be stored, separated by newlines")

(defvar filedb-root-dir (if at-home-p "~/Projects" "a:/Projects")
	"directory path of where filedb-update will begin its search. Place links or symlinks under it.")

(defvar filedb-exclude-dirs '("." ".." "node_modules" ".git" "public" "vendor" "build" "qmk_firmware")
	"list of directory names that filedb-update will ignore and not traverse while building the quick find filedb")

(defun filedb-walk (root disallowed f)
	"Walk the directory ROOT. Do not visit directories in the DISALLOWED list. Then, each directory or file is passed to the callback function F.
You should probably include \".\" and \"..\" in DISALLOWED."
	(ignore-errors
	(dolist (name (directory-files root))
		(when (not (member name disallowed))
			(let ((pathname (concat root "/" name)))
			(if (file-directory-p pathname)
				(filedb-walk pathname disallowed f)
				;; (if (file-directory-p (concat pathname "/.git"))
				;; 	(with-temp-dir pathname
				;; 		(-> "git ls-tree -r --name-only HEAD"
				;; 		(shell-command-to-string $)
				;; 		(string-trim $)
				;; 		(split-string $ "\n")
				;; 		(mapcar (L x (concat pathname "/" x)) $)
				;; 		(seq-each (L x (funcall f x)) $)))
				;; 	(filedb-walk pathname disallowed f))
				(funcall f pathname)))))))

(defun filedb-update ()
	"Update the filedb file. The name of the filedb file is determined in the `filedb' variable.
Begin walking from `filedb-root-dir' and exclude directories in `filedb-exclude-dirs'."
	(interactive)
	(with-temp-file filedb
	(filedb-walk filedb-root-dir filedb-exclude-dirs
		(L x (insert (substring x (+ 1 (length filedb-root-dir)) (length x)) "\n")))))

(defun filedb-find-file ()
	"find-file by searching the filedb file. filedb is a newline-separated list of files.
Update the filedb through `filedb-update' periodically."
	(interactive)
	(-> (slurp filedb)
	(split-string $ "\n")
	(ido-completing-read "select file: " $)
	(concat filedb-root-dir "/" $)
	(find-file $)))
