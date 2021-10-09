;;; -*- lexical-binding: t -*-
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'iso-transl)
(require 'json)
(require 'seq)
(require 'xml)

(dolist (x '("defaults" "functions" "xml" "interactive" "vi-mode" "keys" "hooks" "neocities"))
	(load-file (format "%s/%s.el" user-emacs-directory x)))
