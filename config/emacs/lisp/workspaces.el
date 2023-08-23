(defvar workspaces--alist '() "available workspaces and their hooks")
(defvar workspace-current nil "the current workspace")

(defun workspace--find (name)
  "find the proper workspace cons cell from the given file 'name'"
  (seq-find
   (L x (string-prefix-p (car x) name))
   workspaces--alist))

(defun workspace--configure ()
  "select and run the correct workspace hook depending on the currently open file's location"
  (whenlet w (workspace--find (buffer-file-name))
	   (setq-local workspace-current (car w))
	   (funcall (cdr w))))

(defun workspace-register (path f)
  "register a new workspace"
  (add-to-list 'workspaces--alist (cons path f)))

(add-hook 'find-file-hook 'workspace--configure)

(provide 'workspaces)
