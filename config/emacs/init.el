;; -*- lexical-binding: t -*-
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'iso-transl)
(require 'json)
(require 'seq)

(dolist (x '("defaults" "functions" "interactive" "vi-mode" "keys" "hooks" "serialisation" "neocities"))
	(load-file (format "%s/%s.el" user-emacs-directory x)))

(custom-set-variables
	'(package-selected-packages '(markdown-mode php-mode)))
(custom-set-faces
	'(default ((t (:inherit nil :stipple nil :background "#ffeedd" :foreground "#000000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :family "JuliaMono"))))
	'(cursor ((t (:background "#ff0055"))))
	'(eshell-ls-directory ((t (:foreground "#2255aa" :weight bold))))
	'(eshell-ls-executable ((t (:foreground "#008844" :weight bold))))
	'(eshell-prompt ((t (:foreground "#ff0055" :weight bold))))
	'(font-lock-builtin-face ((t (:underline (:color foreground-color)))))
	'(font-lock-comment-face ((t (:foreground "#aa4422"))))
	'(font-lock-type-face ((t (:foreground "#d33682"))))
	'(font-lock-constant-face ((t (:inherit font-lock-type-face))))
	'(font-lock-function-name-face ((t (:foreground "#2255aa" :weight bold))))
	'(font-lock-keyword-face ((t (:weight semi-bold))))
	'(font-lock-string-face ((t (:foreground "#008844"))))
	'(font-lock-type-face ((t :foreground "#884488")))
	'(font-lock-variable-name-face ((t nil)))
	'(fringe ((t nil)))
	'(cursor ((t (:background "#ff0055"))))
	'(region ((t (:background "#ffffff"))))
	'(highlight ((t (:background "#ccccff"))))
	'(isearch ((t (:background "#2255aa" :foreground "#ffeedd"))))
	'(lazy-highlight ((t (:inherit highlight))))
	'(minibuffer-prompt ((t (:foreground "#ff0055"))))
	'(mmm-default-submode-face ((t nil)))
	'(mode-line ((t (:background "#ddddcc"))))
	'(mode-line-inactive ((t nil)))
	'(php-$this ((t (:slant oblique))))
	'(php-$this-sigil ((t (:inherit php-$this))))
	'(php-function-call ((t (:inherit font-lock-function-name-face))))
	'(show-paren-match ((t (:inherit highlight))))
	'(variable-pitch ((t (:height 190 :family "Sans Serif")))))
