;;; init-magit.el --- Magit settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package magit)

(use-package diff-hl
  :hook
  ((magit-pre-refresh  . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (global-diff-hl-mode t))

(provide 'init-magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-magit.el ends here
