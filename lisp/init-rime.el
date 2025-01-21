;;; init-rime.el --- Rime settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package rime
  :ensure t
  :custom
  (default-input-method 'rime)
  :config
  (setq rime-user-data-dir "~/.config/fcitx/rime")
  (setq rime-show-candidate 'posframe)
  (when (eq system-type 'darwin)
    (setq rime-emacs-module-header-root "/opt/homebrew/include")
    (setq rime-librime-root "/opt/homebrew/opt/librime"))
  ; 临时英文模式
  (setq rime-disable-predicates
	'(meow-motion-mode-p
	  meow-normal-mode-p
	  meow-keypad-mode-p
	  ; 在英文字符串之后, 必须以字母开头的英文字符串
	  rime-predicate-after-alphabet-char-p
	  ; 在 prog-mode 和 conf-mode 中除注释和引号内字符串之外的区域
	  rime-predicate-prog-in-code-p
	  ; 激活 ace-window
	  rime-predicate-ace-window-p
	  ; 当要输入的是符号时
	  rime-predicate-current-input-punctuation-p
	  ; 在中文字符且有空格之后
	  rime-predicate-space-after-cc-p
	  ; 将要输入的是大写字母时
	  rime-predicate-current-uppercase-letter-p
	  ; 在 LaTeX 数学环境中或者输入 LaTeX 命令时
	  rime-predicate-tex-math-or-command-p
	  )
	)
  ; 强制中文模式, 无视 rime-disable-predicates 中的规则
  (define-key rime-mode-map (kbd "M-j") 'rime-force-enable)
  )

(provide 'init-rime)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rime.el ends here
