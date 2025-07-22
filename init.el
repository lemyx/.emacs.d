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
(require 'init-font)
(require 'init-ui)
;(require 'init-treemacs)
(require 'init-save)
(require 'init-window)
(require 'init-ai)
(require 'init-rime)

;; Edit
(require 'init-edit)
(require 'init-completion)
(require 'init-meow)
(require 'init-dired)

;; Reading
(require 'init-english)

;; Coding
(require 'init-prog)
(require 'init-magit)
(require 'init-lsp)
(require 'init-env)
(require 'init-eat)

;; Writing
(require 'init-org)
(require 'init-beancount)
(require 'init-blog)
(require 'init-gtd)
(require 'init-diary)
(require 'init-tex)
(require 'init-pdf)

;; Gtd
(require 'init-calendar)
(require 'init-pomodoro)

; (require 'init-session)
; (emacs-session-restore)

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
