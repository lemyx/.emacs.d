;; init-english.el --- English settings -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; immersive-translate
(use-package immersive-translate
  :ensure t
  :hook
  ((elfeed-show-mode-hook . immersive-translate-setup)
   (Info-mode-hook . immersive-translate-setup)
   (nov-pre-html-render-hook . immersive-translate-setup))
  :config
  (setq immersive-translate-backend 'chatgpt)
  (setq immersive-translate-chatgpt-host "api.deepseek.com")
  (setq immersive-translate-chatgpt-model "deepseek-chat")
  (setq immersive-translate-failed-message "🧐")
  (add-hook 'Info-mode-hook 'immersive-translate-auto-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; paw

(provide 'init-english)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-english.el ends here
