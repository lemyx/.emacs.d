;;; init-rime.el --- Rime settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package rime
  :ensure t
  :config
  (setq default-input-method "rime")
  (setq rime-user-data-dir "~/.config/fcitx/rime")
  (setq rime-show-candidate 'posframe)
  (when (eq system-type 'darwin)
    (setq rime-emacs-module-header-root "/opt/homebrew/include")
    (setq rime-librime-root "/opt/homebrew/opt/librime"))
  )

(provide 'init-rime)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rime.el ends here
