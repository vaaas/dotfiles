(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(electric-indent-mode -1)

(define-minor-mode vi-mode
  "Get your foos in the right places."
  :lighter " vi"
  :keymap (let ((map (make-sparse-keymap)))
    (define-key map (kbd "u") 'vi-mode)
    (define-key map (kbd "i") 'left-char)
    (define-key map (kbd "e") 'next-line)
    (define-key map (kbd "o") 'previous-line)
    (define-key map (kbd "a") 'right-char)
    map))

(define-key global-map (kbd "C-s") 'save-buffer)
(define-key global-map (kbd "C-i") 'dabbrev-expand)
(define-key global-map (kbd "C-o") 'find-file)
(define-key global-map (kbd "C-w") 'backward-kill-word)
(define-key global-map (kbd "C-k") 'kill-this-buffer)
(define-key global-map (kbd "C-z") 'undo)
(define-key global-map (kbd "C-0") 'delete-other-windows)
(define-key global-map (kbd "C-a") 'other-window)
(define-key global-map (kbd "C-,") 'previous-buffer)
(define-key global-map (kbd "C-.") 'next-buffer)
(define-key global-map (kbd "M-f") 'execute-extended-command)
(define-key global-map (kbd "C-t") 'switch-to-buffer)
(define-key global-map (kbd "C-x") 'kill-region)
(define-key global-map (kbd "C-c") 'kill-ring-save)
(define-key global-map (kbd "C-v") 'yank)
(define-key global-map (kbd "<escape>") 'vi-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#fff3e0" :foreground "#303030" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "1ASC" :family "Liberation Mono")))))
