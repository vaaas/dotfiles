(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(electric-indent-mode -1)
(ido-mode 1)

(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq tab-stop-list '(4))
(setq frame-resize-pixelwise t)
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))
(setq vc-follow-symlinks t)

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

(define-minor-mode vi-mode
  :lighter " vi"
  :keymap (let ((map (make-sparse-keymap)))
    (define-key map (kbd "u") 'vi-mode)
    (define-key map (kbd "i") 'left-char)
    (define-key map (kbd "e") 'next-line)
    (define-key map (kbd "o") 'previous-line)
    (define-key map (kbd "a") 'right-char)
    (define-key map (kbd "I") 'beginning-of-line-text)
    (define-key map (kbd "E") 'forward-paragraph)
    (define-key map (kbd "O") 'backward-paragraph)
    (define-key map (kbd "A") 'end-of-line)
    (define-key map (kbd "d d") 'kill-whole-line)
    (define-key map (kbd "l") (lambda() (interactive) (line-below) (vi-mode -1)))
    (define-key map (kbd "<escape>") 'do-nothing)
    (define-key map (kbd "w") 'forward-word)
    (define-key map (kbd "b") 'backward-word)
    (define-key map (kbd "c w") (lambda() (interactive) (kill-word) (vi-mode -1)))
    (define-key map (kbd "c b") (lambda() (interactive) (backward-kill-word) (vi-mode -1)))
    (define-key map (kbd "c c") (lambda() (interactive) (beginning-of-line-text) (kill-line) (vi-mode -1)))
    (define-key map (kbd "c t") (lambda() (interactive) (call-interactively 'zap-to-char) (vi-mode -1)))
    (define-key map (kbd "d w") 'kill-word)
    (define-key map (kbd "d b") 'backward-kill-word)
    (define-key map (kbd ".") 'repeat)
    (define-key map (kbd "\\ v p") 'variable-pitch-mode)
    map))

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
(define-key global-map (kbd "C-F") 'rgrep)

(define-key prog-mode-map (kbd "<tab>") 'expand-or-tab)

(define-key minibuffer-local-map (kbd "<escape>") 'abort-recursive-edit)

(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)
(define-key isearch-mode-map (kbd "<return>") 'isearch-exit)

(add-hook 'prog-mode-hook (lambda() (vi-mode 1)))
(add-hook 'minibuffer-setup-hook (lambda() (vi-mode -1)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (markdown-mode vue-mode php-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 '(variable-pitch ((t (:height 240 :family "Sans Serif")))))
