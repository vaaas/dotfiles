; -*- lexical-binding: t -*-
; imports and packages
(require 'package)
(push '("melpa" . "https://melpa.org/packages/") package-archives)
(package-initialize)
(require 'iso-transl)
(require 'json)
(require 'seq)
(require 'xml)
;(require 'lsp-mode)
;(require 'tree-sitter)
;(require 'tree-sitter-langs)

; I have split the init.el file into several more dedicated files. Dynamically load these files
(seq-each #'load-file
(mapcar (lambda (x) (concat user-emacs-directory "/elisp/" x))
(seq-filter (lambda (x) (not (member x (list "." ".."))))
(directory-files (concat user-emacs-directory "/elisp")))))
