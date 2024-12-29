;;; init-beancount.el --- Beancount settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/site-lisp/beancount-mode/")
(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.bean\\'" . beancount-mode))
(add-hook 'beancount-mode-hook
  (lambda () (setq-local electric-indent-chars nil)))
(add-hook 'beancount-mode-hook #'outline-minor-mode)
(define-key beancount-mode-map (kbd "C-c C-n") #'outline-next-visible-heading)
(define-key beancount-mode-map (kbd "C-c C-p") #'outline-previous-visible-heading)

(provide 'init-beancount)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-beancount.el ends here
