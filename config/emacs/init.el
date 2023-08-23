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
(add-hook 'typescript-ts-mode-hook (lambda () (call-interactively 'eglot)))

; files
(setq auto-save-default nil)
(setq make-backup-files nil)

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

; completion
(setq completion-styles '(normal flex))
(setq-default company-idle-delay nil)
(require 'company)
(add-hook 'prog-mode-hook 'company-mode)
(define-key prog-mode-map (kbd "C-s") 'company-complete)
(define-key company-active-map [tab] 'company-complete-selection)
(define-key company-active-map (kbd "C-s") 'company-complete-selection)
(define-key company-active-map (kbd "C-i") 'company-select-next)
(define-key company-active-map (kbd "C-e") 'company-select-previous)

; isearch
(define-key isearch-mode-map [tab] 'isearch-repeat-forward)
(define-key isearch-mode-map [backtab] 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-i") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-e") 'isearch-repeat-backward)
