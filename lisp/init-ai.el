;;; init-ai.el --- AI settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defun deepseek-api-key-path ()
  "Return the path to the DeepSeek API key file."
  (expand-file-name "~/.dotfiles/llm/deepseek/api_key.txt"))

(defun read-llm-api-key (path)
  "Read and return the API key from the specified file path."
  (with-temp-buffer
    (insert-file-contents path)
    (string-trim (buffer-string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gptel
(use-package gptel
  :ensure t
  :config
  (setq gptel-model 'deepseek-chat)
  (setq gptel-default-mode 'org-mode)
  (setq gptel-backend (gptel-make-openai "DeepSeek"
					:host "api.deepseek.com"
					:endpoint "/chat/completions"
					:stream t
					:key (read-llm-api-key (deepseek-api-key-path))
					:models '("deepseek-chat" "deepseek-coder")))
  ; Let window scroll automatically as the response is inserted
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  ; Let cursor move to the next prompt after the response is inserted
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; aider.el
(add-to-list 'load-path "~/.emacs.d/site-lisp/aider/")
(require 'aider)
(setq aider-args '("--no-auto-commits" "--model" "deepseek/deepseek-coder"))
(setenv "DEEPSEEK_API_KEY" (read-llm-api-key (deepseek-api-key-path)))

(provide 'init-ai)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
