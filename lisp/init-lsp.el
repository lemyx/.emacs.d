;;; init-lsp.el --- LSP settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(use-package yasnippet
  :init
  (yas-global-mode 1))

(add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-bridge")
(require 'lsp-bridge)
(global-lsp-bridge-mode)
(setq acm-enable-doc t)
(setq acm-enable-copilot nil)
(setq lsp-bridge-enable-hover-diagnostic t)
(setq lsp-bridge-enable-completion-in-string t)
(setq lsp-bridge-enable-completion-in-minibuffer t)
(setq lsp-bridge-enable-with-tramp t)
(setq lsp-bridge-python-lsp-server "pyright")
(setq lsp-bridge-tex-lsp-server "texlab")
(setq lsp-bridge-python-command "/usr/bin/python3")

(one-key-create-menu
 "LSP"
 '((("a" . "lsp-bridge-code-action") . lsp-bridge-code-action)
   (("f" . "lsp-bridge-find-def") . lsp-bridge-find-def)
   (("b" . "lsp-bridge-find-def-return") . lsp-bridge-find-def-return)
   (("h" . "lsp-bridge-popup-documentation") . lsp-bridge-popup-documentation)
   (("s" . "lsp-bridge-toggle-sdcv-helper") . lsp-bridge-toggle-sdcv-helper))
 t)

(provide 'init-lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
