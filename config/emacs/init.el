;;; -*- lexical-binding: t; -*-

; custom
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#ffeedd" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 140 :width normal :foundry "ADBO" :family "Source Code Pro")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(company)))

; macros
(defmacro L (arg &rest body) `(lambda (,arg) ,@body))

(defmacro whenlet (var def &rest body) `(let ((,var ,def)) (when ,var ,@body)))

(defmacro -> (initial &rest forms)
  (seq-reduce
   (lambda (acc x) (append (if (listp x) x (list x)) (list acc)))
   forms initial))

(defmacro define-keys (mode &rest forms)
    `(progn ,@(mapcar (lambda (x) `(define-key ,mode ,@x)) forms)))

; configure packages
(push (concat user-emacs-directory "/lisp") load-path)
(require 'workspaces)

; normal mode
(require 'normal-mode)
(define-key global-map (kbd "C-g") 'normal-mode-on)
(add-hook 'prog-mode-hook 'normal-mode-on)

; keyboard bindings
(define-key global-map (kbd "C-t") 'other-window)
(define-key prog-mode-map (kbd "TAB") 'indent-for-tab-command)
(define-key global-map (kbd "C-z") nil)

; typescript
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

; files
(setq auto-save-default nil)
(setq make-backup-files nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq vc-follow-symlinks t)

; tree-sitter
(setq
 treesit-language-source-alist
 '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
   (cmake      "https://github.com/uyha/tree-sitter-cmake")
   (css        "https://github.com/tree-sitter/tree-sitter-css")
   (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
   (go         "https://github.com/tree-sitter/tree-sitter-go")
   (html       "https://github.com/tree-sitter/tree-sitter-html")
   (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
   (json       "https://github.com/tree-sitter/tree-sitter-json")
   (make       "https://github.com/alemuller/tree-sitter-make")
   (markdown   "https://github.com/ikatyang/tree-sitter-markdown")
   (python     "https://github.com/tree-sitter/tree-sitter-python")
   (toml       "https://github.com/tree-sitter/tree-sitter-toml")
   (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
   (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
   (yaml       "https://github.com/ikatyang/tree-sitter-yaml")))

; visuals
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(undecorated . t))
(fringe-mode 0)
(setq-default line-spacing 4)
(global-visual-line-mode 1)
(setq-default mode-line-format nil)
(setq ring-bell-function 'silent)

; completion
(setq completion-styles '(normal flex))
(setq-default company-idle-delay nil)
(require 'company)
(add-hook 'prog-mode-hook 'company-mode)
(define-key prog-mode-map (kbd "C-s") 'company-complete)
(define-keys company-active-map
    ([tab] 'company-complete-selection)
    ((kbd "C-s") 'company-complete-selection)
    ((kbd "C-i") 'company-select-previous)
    ((kbd "C-e") 'company-select-next)
    ((kbd "C-o") 'company-select-previous)
    ((kbd "C-a") 'company-select-next))

; minibuffer completion / icomplete
(icomplete-mode 1)
(icomplete-vertical-mode 1)
(setq-default icomplete-prospects-height 10)
(define-keys icomplete-vertical-mode-minibuffer-map
    ([tab] 'icomplete-force-complete)
    ([return] 'icomplete-force-complete-and-exit)
    ((kbd "C-s") 'icomplete-force-complete-and-exit)
    ((kbd "C-i") 'icomplete-backward-completions)
    ((kbd "C-e") 'icomplete-forward-completions)
    ((kbd "C-o") 'icomplete-backward-completions)
    ((kbd "C-a") 'icomplete-forward-completions))

; isearch
(define-keys isearch-mode-map
    ([tab] 'isearch-repeat-forward)
    ([backtab] 'isearch-repeat-backward)
    ((kbd "C-i") 'isearch-repeat-backward)
    ((kbd "C-e") 'isearch-repeat-forward)
    ((kbd "C-o") 'isearch-repeat-backward)
    ((kbd "C-a") 'isearch-repeat-forward))

; indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default typescript-ts-mode-indent-offset tab-width)
(setq indent-line-function 'insert-tab)
