; -*- lexical-binding: t -*-

(defvar file-db (expand-file-name "~/filedb.txt")
	"file name for the quick find file db. This is where the results will be stored, separated by newlines")

(defvar file-db-root-dir "~/Projects"
	"directory path of where update-file-db will begin its search. Place links or symlinks under it.")

(defvar file-db-exclude-dirs '("." ".." "node_modules" ".git" "public" "vendor" "build")
	"list of directory names that update-file-db will ignore and not traverse while building the quick find file-db")

(defun filedb-walk (root disallowed f)
	"Walk the directory ROOT. Do not visit directories in the DISALLOWED list. Then, each directory or file is passed to the callback function F.

You should probably include \".\" and \"..\" in DISALLOWED."
	(dolist (name (directory-files root))
		(when (not (member name disallowed))
			(let ((pathname (concat root "/" name)))
			(if (file-directory-p pathname)
				(filedb-walk pathname disallowed f)
				(funcall f pathname))))))

(defun update-file-db () (interactive)
	(with-temp-file file-db (filedb-walk
		file-db-root-dir
		file-db-exclude-dirs
		(lambda(x) (insert x "\n")))))

(defun quick-find-file () (interactive)
	(find-file
	(concat file-db-root-dir "/"
	(ido-completing-read "select file> "
	(mapcar (lambda (x) (substring x (+ 1 (length file-db-root-dir)) (length x)))
	(split-string
	(slurp file-db)
	"\n"))))))
