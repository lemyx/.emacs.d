;;; init.el --- The main init entry for Emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; load paths
(dolist (folder '("lisp" "site-lisp"))
  (add-to-list 'load-path (expand-file-name folder user-emacs-directory)))

;; stop emacs automatically editing .emacs
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Generic
(require 'init-package)
(require 'init-onekey)
(require 'init-ui)
(require 'init-treemacs)
(require 'init-save)
(require 'init-window)

;; Edit
(require 'init-completion)
(require 'init-meow)
(require 'init-dired)

;; Coding
(require 'init-prog)
(require 'init-magit)
(require 'init-lsp)
(require 'init-env)
(require 'init-vterm)

;; Writing
(require 'init-org)
(require 'init-beancount)
(require 'init-blog)
(require 'init-gtd)
(require 'init-diary)
(require 'init-tex)
(require 'init-pdf)

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
