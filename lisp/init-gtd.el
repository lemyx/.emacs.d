;;; init-gtd.el --- GTD settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-static-blog for blog
(use-package org-gtd
  :init
  (setq org-gtd-update-ack "3.0.0")
  :after org
  :demand t
  :custom
  (org-gtd-directory "~/org/gtd")
  (org-edna-use-inheritance t)
  (org-gtd-organize-hooks '(org-gtd-set-area-of-focus org-set-tags-command))
  :config
  (org-edna-mode)
  :bind
  (("C-c d c" . org-gtd-capture)
   ("C-c d e" . org-gtd-engage)
   ("C-c d p" . org-gtd-process-inbox)
   :map org-gtd-clarify-map
   ("C-c c" . org-gtd-organize)))

(provide 'init-gtd)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-gtd.el ends here
