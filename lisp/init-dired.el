;;; init-dired.el --- dired settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; allow dired to delete or copy dir
;; 'top means ask every time
;; 'always means no asking
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)

;; smart suggest target directory
(setq dired-dwim-target t)

;; make dired use the same buffer for viewing directory
(setq dired-kill-when-opening-new-dired-buffer t)

;; show human readable file size
;; https://github.com/d12frosted/homebrew-emacs-plus/issues/383#issuecomment-899157143
(when (eq system-type 'darwin)
  (setq insert-directory-program "gls"))
(when (eq system-type 'gnu/linux)
  (setq insert-directory-program "ls"))
(setq dired-use-ls-dired t)
(setq dired-listing-switches "-al --group-directories-first")

;; (use-package nerd-icons-dired
;;   :hook
;;   (dired-mode . nerd-icons-dired-mode))

(provide 'init-dired)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
