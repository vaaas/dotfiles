; -*- lexical-binding: t -*-
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'iso-transl)
(require 'json)
(require 'seq)
(require 'xml)

; I have split the init.el file into several more dedicated files. Dynamically load these files
(dolist (x (directory-files (concat user-emacs-directory "/elisp" )))
	(load-file (concat user-emacs-directory "/elisp/" x)))
