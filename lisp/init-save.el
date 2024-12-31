;;; init-save.el --- Save settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-save
(add-to-list 'load-path "~/.emacs.d/site-lisp/auto-save/")
(require 'auto-save)
(auto-save-enable)
(setq auto-save-silent t)

(provide 'init-save)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-save.el ends here
