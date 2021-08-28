;; -*- lexical-binding: t -*-
(defalias 'first 'car)

(defalias 'rest 'cdr)

(defun backward-whitespace() (interactive) (forward-whitespace -1))

(defun zap-up-to-char-backward() (interactive)
	(zap-up-to-char -1 (read-char "zap to char backward")))

(defun quick-goto-char() (interactive)
	(search-forward (make-string 1 (read-char "go to char"))))

(defun quick-goto-char-backward() (interactive);
	(search-backward (make-string 1 (read-char "go to char backward"))))

(defun insert-indent()
	(if indent-tabs-mode (insert-char 9) (insert-char 32 tab-width)))

(defun delete-indent()
	(cond
		((and indent-tabs-mode (= 9 (char-after)))
			(delete-forward-char 1))
		((and
			(not indent-tabs-mode)
			(string=
				(make-string tab-width 32)
				(buffer-substring (point) (+ (point) tab-width))))
			(delete-forward-char tab-width))
		(t (do-nothing))))

(defun expand-or-tab() (interactive)
	(if (member (char-before) '(9 10 32))
		(insert-indent)
		(call-interactively 'dabbrev-expand)))

(defun line-below() (interactive)
	(end-of-line) (newline-and-indent-relative))

(defun line-above() (interactive)
	(save-excursion
		(beginning-of-line)
		(newline)
		(forward-line -1)
		(indent-relative)))

(defun newline-and-indent-relative() (interactive) (newline) (indent-relative t t))

(defun do-nothing() (interactive))
(put 'do-nothing 'no-self-insert t)

(defun add-trailing-newline() (end-of-buffer) (when (not (= 10 (char-before))) (insert-char 10)))

(defun add-trailing-newline() (end-of-buffer) (when (not (= 10 (char-before))) (insert-char 10)))

(defun filedb-walk(root disallowed f)
	(dolist (name (directory-files root))
		(when (not (member name disallowed))
			(setq pathname (concat root "/" name))
			(if (file-directory-p pathname)
				(filedb-walk pathname disallowed f)
				(funcall f pathname)))))

(defun update-file-db() (interactive)
	(with-temp-file file-db (filedb-walk
		(if (string= system-type "windows-nt") "a:/code" "~/Projects")
		'("." ".." "node_modules" ".git" "public" "vendor" "build")
		(lambda(x) (insert x "\n")))))

(defun quick-find-file() (interactive)
	(find-file
	(ido-completing-read "select file> "
	(split-string
	(slurp file-db)
	"\n"))))

(defun slurp(f)
	(with-temp-buffer
		(insert-file-contents f)
		(buffer-substring-no-properties (point-min) (point-max))))

(defun dired-here() (interactive) (dired default-directory))

(defun vi-on() (interactive)
	(vi-mode 1)
	(setq cursor-type 'box))

(defun vi-off() (interactive)
	(vi-mode -1)
	(setq cursor-type 'bar))

(defun backspace-or-unindent() (interactive)
	(cond
		((use-region-p) (call-interactively 'kill-region))
		((< (point) (+ 1 tab-width)) (backward-delete-char 1))
		(indent-tabs-mode (backward-delete-char 1))
		((string= (make-string tab-width 32) (buffer-substring (point) (- (point) tab-width)))
			(backward-delete-char tab-width))
		(t (backward-delete-char 1))))

(defun double-newline() (interactive)
	(cond
		((= 0 (point)) (newline))
		((= 10 (char-before)) (newline))
		(t (newline) (newline))))

(defun toggle-indent-tabs() (interactive)
	(if indent-tabs-mode
		(progn (setq indent-tabs-mode nil) (setq tab-width 4) (message "indent will use SPACES"))
		(progn (setq indent-tabs-mode t) (setq tab-width 3) (message "indent will use TABS"))))

(defun replace-all (from to)
	(beginning-of-buffer)
	(while (search-forward from nil t)
		(replace-match to t t)))

(defun replace-all-regex (from to)
	(beginning-of-buffer)
	(while (re-search-forward from nil t)
		(replace-match to t nil)))

(defun french() (interactive)
	(replace-all-regex "\"\\([^\"]+?\\)\"" "« \\1 »")
	(replace-all "?!" "⁈")
	(replace-all-regex "[ |\u202F]+\\(;\\|!\\|\\?\\|⁈\\)" "\u202F\\1")
	(replace-all-regex "[ |\u00A0]+\\([:\\|»]\\)" "\u00A0\\1")
	(replace-all-regex "«[ |\u00A0]+" "«\u00A0"))

(defun eval-region-smart() (interactive)
	(shell-command-on-region (point-min) (point-max) eval-process))

(defun shell-command-this-buffer(x) (interactive)
	(shell-command-on-region (point-min) (point-max) x (current-buffer) t))

(defun cmark() (interactive)
	(shell-command-this-buffer "cmark --smart --unsafe"))

(defun spaces-to-tabs() (interactive)
	(beginning-of-buffer)
	(replace-all (make-string tab-width 32) "	"))

(defun join-line() (interactive)
	(end-of-line)
	(forward-line)
	(beginning-of-line-text)
	(delete-indentation))

(defun indent-line-or-region() (interactive)
	(if (use-region-p)
		(progn
			(call-interactively 'indent-rigidly-right-to-tab-stop)
			(setq deactivate-mark nil))
		(save-excursion (beginning-of-line) (insert-indent))))

(defun unindent-line-or-region() (interactive)
	(if (use-region-p)
		(progn
			(call-interactively 'indent-rigidly-left-to-tab-stop)
			(setq deactivate-mark nil))
		(save-excursion (beginning-of-line) (delete-indent))))

(defun timestamp() (format-time-string "%s"))

(defun blog() (interactive)
	(setq cat (ido-completing-read "category?> "
		'("tech" "anime" "books" "memes" "films" "journal" "games")))
	(setq stamp (timestamp))
	(delete-trailing-whitespace)
	(beginning-of-buffer)
	(if (< (buffer-size) 2000)
		(insert (format "<post timestamp='%s' tag='%s'>" stamp cat))
	(progn
		(setq file-name (concat
			(replace-regexp-in-string " " "_"
				(read-string "file name (no extension): "))
			".html"))
		(insert (format "<post filename='%s' timestamp='%s' tag='%s'>" file-name stamp cat))))
	(add-trailing-newline)
	(insert "</post>")
	(cd blog-directory)
	(shell-command-this-buffer "python3 ncrender")
	(cd "~"))

(defun b-then-a(a b &rest args) (lambda() (interactive) (apply b args) (funcall a)))

(defun xinit()
	(start-process "Xorg" "Xorg" "/usr/bin/Xorg" (concat ":" tty) "-auth" "/tmp/Xauthority" "-noreset" "-keeptty" (concat "vt" tty))
	(sit-for 0.2)
	(xinitrc))

(defun xinitrc()
	(setenv "BROWSER" "firefox")
	(setenv "EDITOR" "emacs")
	(setenv "GDK_CORE_DEVICE_EVENTS" "1")
	(setenv "GTK_USE_PORTAL" "1")
	(setenv "GTK_IM_MODULE" "ibus")
	(setenv "QT_IM_MODULE" "ibus")
	(setenv "XMODIFIERS" "@im=ibus")
	(call-process "xrdb" nil nil nil "-merge" "~/.Xresources")
	(call-process "xset" nil nil nil "r" "rate" "180")
	(call-process "setxkbmap" nil nil nil "slatin")
	(start-process "ibus" "ibus" "ibus-daemon" "-xr")
	(start-process "dunst" "dunst" "dunst" "-font" "Sans 16")
	(start-process "sxhkd" "sxhkd" "dbus-launch" "sxhkd" "-m" "-1")
	(start-process "redshift-service")
	(start-process "xwallpaper" "xwallpaper" "xwallpaper" "--tile" "wallpaper")
	(start-process "unclutter" "unclutter" "unclutter" "--timeout" "1")
	(start-process "bspwm" "bspwm" "dbus-launch" "bspwm"))
