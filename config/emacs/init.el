; -*- lexical-binding: t -*-
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'iso-transl)
(require 'json)
(require 'seq)
(require 'xml)

; I have split the init.el file into several more dedicated files. Dynamically load these files
(seq-each (lambda (x) (load-file (concat user-emacs-directory "/" x)))
(seq-filter (lambda (x) (and (string-suffix-p ".el" x) (not (string= "init.el" x))))
(directory-files user-emacs-directory)))
