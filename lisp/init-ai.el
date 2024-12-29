;;; init-ai.el --- AI settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package gptel
  :ensure t
  :config
  (setq gptel-model "deepseek-chat")
  (setq gptel-default-mode 'org-mode)
  (setq	(gptel-make-openai "DeepSeek"
	  :host "api.deepseek.com"
	  :endpoint "/chat/completions"
	  :stream t
	  :key (with-temp-buffer
		 (insert-file-contents "~/.config/llm/deepseek/api_key.txt")
		 (string-trim (buffer-string)))
	  :models '("deepseek-chat" "deepseek-coder"))))

;; aider.el
(add-to-list 'load-path "~/.emacs.d/site-lisp/aider/")
(require 'aider)
(setq aider-args '("--model" "deepseek/deepseek-coder"))
(setenv "DEEPSEEK_API_KEY" (with-temp-buffer
                               (insert-file-contents "~/.config/llm/deepseek/api_key.txt")
                               (string-trim (buffer-string))))

(provide 'init-ai)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
