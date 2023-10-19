;;; -*- lexical-binding: t; -*-

; interactive functions
(defun quick-goto-char ()
  (interactive)
  (search-forward (make-string 1 (read-char "go to char"))))

(defun quick-goto-char-backward ()
  (interactive)
  (search-backward (make-string 1 (read-char "go to char backward"))))

(defun visual-select ()
  (interactive)
  (if (use-region-p)
      (deactivate-mark)
    (call-interactively 'set-mark-command)))

(defun visual-line-select ()
  (interactive)
  (beginning-of-line)
  (call-interactively 'set-mark-command)
  (forward-line))

(defun zap-up-to-char-backward ()
   "`zap-up-to-char' with negative argument"
  (interactive)
  (zap-up-to-char -1 (read-char "zap to char backward")))

(defun delete-or-unindent ()
  (interactive)
  (cond
   ((use-region-p) (call-interactively 'kill-region))
   (t (call-interactively 'backward-delete-char-untabify))))

(defun next-buffer-and-normal-mode-on ()
  (interactive)
  (next-buffer)
  (normal-mode-on))

(defun previous-buffer-and-normal-mode-on ()
  (interactive)
  (previous-buffer)
  (normal-mode-on))

; normal mode
(define-minor-mode normal-editing-mode
  "minor mode for mimicking vi-like modal input"
  :lighter " N"
  :keymap
  '(([tab] . next-buffer-and-normal-mode-on)
    ([backtab] . previous-buffer-and-normal-mode-on)
    ("c" . kill-whole-line)
    ("q" . kill-this-buffer)
    ("j" . join-line)
    ("r" . zap-up-to-char)
    ("R" . zap-up-to-char-backward)
    ("s" . visual-select)
    ("S" . visual-line-select)
    ("t" . quick-goto-char)
    ("T" . quick-goto-char-backward)
    ("n" . delete-or-unindent)
    ("N" . delete-forward-char)
    ("i" . left-char)
    ("I" . beginning-of-line-text)
    ("e" . next-line)
    ("E" . forward-paragraph)
    ("o" . previous-line)
    ("O" . backward-paragraph)
    ("a" . right-char)
    ("A" . end-of-line)
    ("z" . undo-only)
    ("Z" . undo-redo)
    ("f" . project-find-file)
    ("F" . switch-to-buffer)
    ("g" . beginning-of-buffer)
    ("G" . end-of-buffer)
    ("p" . yank)
    ("/" . isearch-forward)
    (" " . normal-mode-off)))

(defun normal-mode-on () (interactive) (normal-editing-mode 1))

(defun normal-mode-off () (interactive) (normal-editing-mode -1))

(provide 'normal-mode)
