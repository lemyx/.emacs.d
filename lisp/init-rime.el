;;; init-rime.el --- Rime settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package rime
  :ensure t
  :config
  (setq default-input-method "rime")
  (setq rime-user-data-dir "~/.config/fcitx/rime")
  (setq rime-show-candidate 'posframe)
  :bind
  ("C-SPC" . 'toggle-input-method)
  )

(provide 'init-rime)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rime.el ends here
