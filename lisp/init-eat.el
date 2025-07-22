;;; init-eat.el --- eat settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package eat
  :ensure nil
  :demand t
  :load-path "~/.emacs.d/site-lisp/emacs-eat"
  :hook
  (
   ; Run Eat inside Eshell
   (eshell-load-hook . eat-eshell-mode)
   ; Run visual commands with Eat instead of Term
   (eshell-load-hook . eat-eshell-visual-command-mode))
  )

(provide 'init-eat)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eat.el ends here
