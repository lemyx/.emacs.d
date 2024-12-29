;;; init-ai.el --- AI settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defun deepseek-api-key-path ()
  "Return the path to the DeepSeek API key file."
  (expand-file-name "~/.config/llm/deepseek/api_key.txt"))

(defun read-deepseek-api-key ()
  "Read and return the DeepSeek API key from the configured file."
  (with-temp-buffer
    (insert-file-contents (deepseek-api-key-path))
    (string-trim (buffer-string))))

(use-package gptel
  :ensure t
  :config
  (setq gptel-model "deepseek-chat")
  (setq gptel-default-mode 'org-mode)
  (setq (gptel-make-openai "DeepSeek"
          :host "api.deepseek.com"
          :endpoint "/chat/completions"
          :stream t
          :key (read-deepseek-api-key)
          :models '("deepseek-chat" "deepseek-coder"))))

;; aider.el
(add-to-list 'load-path "~/.emacs.d/site-lisp/aider/")
(require 'aider)
(setq aider-args '("--model" "deepseek/deepseek-coder"))
(setenv "DEEPSEEK_API_KEY" (read-deepseek-api-key))

(provide 'init-ai)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
