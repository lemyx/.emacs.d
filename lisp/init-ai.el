;;; init-ai.el --- AI settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gptel
(defun deepseek-api-key-path ()
  "Return the path to the DeepSeek API key file."
  (expand-file-name "~/.dotfiles/llm/deepseek/api_key.txt"))

(defun read-llm-api-key (path)
  "Read and return the API key from the specified file path."
  (with-temp-buffer
    (insert-file-contents path)
    (string-trim (buffer-string))))

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
;;; claude-code
(add-to-list 'display-buffer-alist
                 '("^\\*claude"
                   (display-buffer-in-side-window)
                   (side . right)
                   (window-width . 90)))

(use-package claude-code
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  (progn
    (setenv "ANTHROPIC_BASE_URL" "http://localhost:8082")
    (setenv "ANTHROPIC_AUTH_TOKEN" "api-key")
    (setq claude-code-program "/usr/local/bin/claude")
    )
  (setq claude-code-terminal-backend 'eat)
  (setq claude-code-term-name "xterm-256color")
  (setq claude-code-program-switches '("--verbose"))
  (setq claude-code-enable-notifications t)
  (setq claude-code-notification-function 'claude-code--default-notification)
  )

(one-key-create-menu
 "CLAUDECODE"
 '((("a" . "claude-code-transient") . claude-code-transient))
 t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; copilot
(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest :branch "main")
  :hook
  (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
	      ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  )

(provide 'init-ai)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
