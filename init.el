;; -*- lexical-binding: t; -*-
(setq
    gc-cons-threshold most-positive-fixnum
    gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook (lambda () (setq
    gc-cons-threshold 16777216
    gc-cons-percentage 0.1)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(electric-indent-mode -1)
(ido-mode 1)
(global-visual-line-mode 1)
(abbrev-mode 1)

(setq-default indent-tabs-mode nil
    mode-line-format nil)
(setq inhibit-splash-screen t
    inhibit-startup-message t
    tab-width 4
    tab-stop-list '(4 8 12 16 20 24 28 32 36 40)
    frame-resize-pixelwise t
    vc-follow-symlinks t
    make-backup-files nil)
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))

(put 'dired-find-alternate-file 'disabled nil)

(defun expand-or-tab()
    (interactive)
    (if (member (char-before) '(10 32))
        (tab-to-tab-stop)
        (call-interactively 'dabbrev-expand)))

(defun line-below()
    (interactive)
    (end-of-line) (open-line 1) (next-line))

(defun do-nothing() (interactive))

(defun quick-find-file()
    (interactive)
    (find-file
    (ido-completing-read "select file> "
    (split-string
    (shell-command-to-string "xzcat ~/filedb.xz") "\n"))))

(defun dired-here() (interactive) (dired default-directory))

(defun backspace-or-unindent() (interactive)
    (if (string= "    " (buffer-substring (point) (- (point) 4)))
        (backward-delete-char 4)
        (backward-delete-char 1)))

(setq vi-mode-map (make-sparse-keymap))
(define-key vi-mode-map (kbd "q") 'kmacro-start-macro)
(define-key vi-mode-map (kbd "Q") 'kmacro-end-or-call-macro)
(define-key vi-mode-map (kbd "u") 'vi-mode)
(define-key vi-mode-map (kbd "i") 'left-char)
(define-key vi-mode-map (kbd "e") 'next-line)
(define-key vi-mode-map (kbd "o") 'previous-line)
(define-key vi-mode-map (kbd "a") 'right-char)
(define-key vi-mode-map (kbd "I") 'beginning-of-line-text)
(define-key vi-mode-map (kbd "E") 'forward-paragraph)
(define-key vi-mode-map (kbd "O") 'backward-paragraph)
(define-key vi-mode-map (kbd "A") 'end-of-line)
(define-key vi-mode-map (kbd "d d") 'kill-whole-line)
(define-key vi-mode-map (kbd "l") (lambda() (interactive) (line-below) (vi-mode -1)))
(define-key vi-mode-map (kbd "<escape>") 'do-nothing)
(define-key vi-mode-map (kbd "w") 'forward-word)
(define-key vi-mode-map (kbd "b") 'backward-word)
(define-key vi-mode-map (kbd "c w") (lambda() (interactive) (kill-word 1) (vi-mode -1)))
(define-key vi-mode-map (kbd "c b") (lambda() (interactive) (backward-kill-word 1) (vi-mode -1)))
(define-key vi-mode-map (kbd "c c") (lambda() (interactive) (beginning-of-line-text) (kill-line) (vi-mode -1)))
(define-key vi-mode-map (kbd "c t") (lambda() (interactive) (call-interactively 'zap-to-char) (vi-mode -1)))
(define-key vi-mode-map (kbd "d t") 'zap-to-char)
(define-key vi-mode-map (kbd "d w") 'kill-word)
(define-key vi-mode-map (kbd "d b") 'backward-kill-word)
(define-key vi-mode-map (kbd ".") 'repeat)
(define-key vi-mode-map (kbd "\\ v p") 'variable-pitch-mode)
(define-key vi-mode-map (kbd "\\ e a") 'edit-abbrevs)
(define-key vi-mode-map (kbd "j e") (lambda() (interactive) (next-line) (beginning-of-line-text) (delete-indentation)))
(define-key vi-mode-map (kbd "v") 'set-mark-command)
(define-key vi-mode-map (kbd "S-<tab>") 'indent-rigidly-left-to-tab-stop)
(define-key vi-mode-map (kbd "<tab>") 'indent-rigidly-right-to-tab-stop)
(define-key vi-mode-map (kbd "<escape>") 'keyboard-quit)
(define-key vi-mode-map (kbd "/") 'isearch-forward)
(define-key vi-mode-map (kbd "?") 'isearch-backward)

(define-minor-mode vi-mode
    "Ghetto vi mode"
    :lighter " vi"
    :keymap 'vi-mode-map)

(define-key global-map (kbd "C-s") 'save-buffer)
(define-key global-map (kbd "C-u") 'vi-mode)
(define-key global-map (kbd "C-o") 'find-file)
(define-key global-map (kbd "C-S-O") 'quick-find-file)
(define-key global-map (kbd "C-w") 'backward-kill-word)
(define-key global-map (kbd "C-k") 'kill-this-buffer)
(define-key global-map (kbd "C-z") 'undo)
(define-key global-map (kbd "C-0") 'delete-other-windows)
(define-key global-map (kbd "C-a") 'other-window)
(define-key global-map (kbd "C-,") 'previous-buffer)
(define-key global-map (kbd "C-.") 'next-buffer)
(define-key global-map (kbd "M-f") 'execute-extended-command)
(define-key global-map (kbd "M-F") 'eval-expression)
(define-key global-map (kbd "C-t") 'switch-to-buffer)
(define-key global-map (kbd "C-x") 'kill-region)
(define-key global-map (kbd "C-c") 'kill-ring-save)
(define-key global-map (kbd "C-v") 'yank)
(define-key global-map (kbd "<escape>") 'vi-mode)
(define-key global-map (kbd "C-<tab>") 'ido-switch-buffer)
(define-key global-map (kbd "C-f") 'isearch-forward)
(define-key global-map (kbd "C-S-F") 'rgrep)
(define-key global-map (kbd "M-`") 'eshell)
(define-key global-map (kbd "<f2>") 'dired-here)

(define-key prog-mode-map (kbd "<tab>") 'expand-or-tab)
(define-key prog-mode-map (kbd "<backspace>") 'backspace-or-unindent)
(define-key text-mode-map (kbd "<tab>") 'expand-or-tab)
(define-key text-mode-map (kbd "<backspace>") 'backspace-or-unindent)

(define-key minibuffer-local-map (kbd "<escape>") 'abort-recursive-edit)

(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)
(define-key isearch-mode-map (kbd "<return>") 'isearch-exit)

(add-hook 'prog-mode-hook (lambda() (vi-mode 1)))
(add-hook 'minibuffer-setup-hook (lambda() (vi-mode -1)))
(add-hook 'text-mode-hook (lambda() (abbrev-mode 1)))
(add-hook 'eshell-mode-hook (lambda()
    (vi-mode -1)
    (define-key eshell-mode-map (kbd "M-`") 'kill-this-buffer)))
(add-hook 'dired-mode-hook (lambda() (dired-hide-details-mode)))

(with-eval-after-load 'markdown-mode
    (setq markdown-mode-map (make-sparse-keymap)))

(with-eval-after-load 'php-mode
    (setq php-mode-map (make-sparse-keymap)))

(with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)
    (define-key dired-mode-map (kbd "C-o") nil))

(custom-set-variables
 '(package-selected-packages (quote (markdown-mode vue-mode php-mode))))
(custom-set-faces
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#fff3e0" :foreground "#303030" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "1ASC" :family "Liberation Mono"))))
 '(font-lock-function-name-face ((t (:foreground "#048"))))
 '(font-lock-keyword-face ((t (:foreground "#a05"))))
 '(font-lock-string-face ((t (:foreground "#084"))))
 '(font-lock-type-face ((t (:foreground "#088"))))
 '(fringe ((t nil)))
 '(isearch ((t (:background "#048" :foreground "#fff"))))
 '(lazy-highlight ((t (:inherit highlight))))
 '(minibuffer-prompt ((t (:foreground "#800"))))
 '(mmm-default-submode-face ((t nil)))
 '(mode-line ((t (:background "#aa8"))))
 '(mode-line-inactive ((t nil)))
 '(show-paren-match ((t (:inherit highlight))))
 '(variable-pitch ((t (:height 240 :family "Sans Serif")))))
