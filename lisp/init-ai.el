;;; init-ai.el --- AI settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defun read-llm-api-key (path)
  "Read and return the API key from the specified file path."
  (with-temp-buffer
    (insert-file-contents path)
    (string-trim (buffer-string))))

(defun deepseek-api-key-path ()
  "Return the path to the DeepSeek API key file."
  (expand-file-name "~/.dotfiles/llm/api_keys/deepseek.txt"))

(defun moonshot-api-key-path ()
  "Return the path to the Moonshot API key file."
  (expand-file-name "~/.dotfiles/llm/api_keys/moonshot.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gptel
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
  (setq claude-code-terminal-backend 'eat)
  (setq claude-code-term-name "xterm-256color")
  (setq claude-code-program-switches '("--verbose"))
  (setq claude-code-enable-notifications t)
  (setq claude-code-notification-function 'claude-code--default-notification)

  ;; 方法一: 通过 claude-code-proxy 的方式设置代理, 接入 OPENAI-compatible 模型
  ;; 启动 claude-code-proxy 参考 https://github.com/sylearn/AICode 跟进该项目以保持服务可用
  ;; (progn
  ;;   (setenv "ANTHROPIC_BASE_URL" "http://192.168.31.174:8082")
  ;;   (setenv "ANTHROPIC_AUTH_TOKEN" "api-key")
  ;;   (setq claude-code-program "/usr/local/bin/claude")
  ;;   )

  ;; 方法二: 使用 kimi Anthropic-compatible API interface
  ;; kimi 提供了 Anthropic-compatible API 接口, 参考链接 https://platform.moonshot.ai/docs/guide/agent-support.en-US#install-cline
  ;; kimi 存在充值与限速问题, 参考链接 https://platform.moonshot.cn/docs/pricing/limits#%E9%99%90%E9%80%9F%E6%A6%82%E5%BF%B5%E8%A7%A3%E9%87%8A
  ;; 可以关注 LLM-Red-Team/kimi-cc 项目中的讨论, 例如 https://github.com/LLM-Red-Team/kimi-cc/issues/35
  ;; TODO: 探索这种使用方式下切换模型
  (progn
    (setenv "ANTHROPIC_BASE_URL" "https://api.moonshot.cn/anthropic/")
    (setenv "ANTHROPIC_AUTH_TOKEN" (read-llm-api-key (moonshot-api-key-path)))
    (setq claude-code-program "/usr/local/bin/claude")
    )

  ;; 方法三: 接入 Qwen3-Coder
  ;; Qwen3-Coder 官方提供了接入 claude-code 和 qwen-code 的方式, 参考链接 https://qwenlm.github.io/blog/qwen3-coder/#claude-code
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
