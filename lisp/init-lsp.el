;;; init-lsp.el --- LSP settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp-bridge
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
(setq lsp-bridge-python-lsp-server "basedpyright")
(setq lsp-bridge-tex-lsp-server "texlab")
(setq lsp-bridge-python-command
      (cond
       ((eq system-type 'gnu/linux) "/usr/bin/python3")
       ((eq system-type 'darwin) "/opt/homebrew/bin/python3")
       ))

(one-key-create-menu
 "LSP"
 '((("a" . "lsp-bridge-code-action") . lsp-bridge-code-action)
   (("f" . "lsp-bridge-find-def") . lsp-bridge-find-def)
   (("b" . "lsp-bridge-find-def-return") . lsp-bridge-find-def-return)
   (("h" . "lsp-bridge-popup-documentation") . lsp-bridge-popup-documentation)
   (("s" . "lsp-bridge-toggle-sdcv-helper") . lsp-bridge-toggle-sdcv-helper))
 t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; virtual environment
(use-package pyvenv
  :ensure t
  :config
  (setenv "WORKON_HOME" (expand-file-name
			 (cond ((eq system-type 'gnu/linux) "~/miniconda3/envs")
			       ((eq system-type 'darwin) "/opt/homebrew/Caskroom/miniconda/base/envs"))))
  (pyvenv-mode t)
  (add-hook 'python-mode-hook
            (lambda () (pyvenv-workon "dev")))
)

(defun local/lsp-bridge-get-single-lang-server-by-project (project-path filepath)
  (let* ((json-object-type 'plist)
         (custom-dir (expand-file-name ".cache/lsp-bridge/pyright" user-emacs-directory))
         (custom-config (expand-file-name "pyright.json" custom-dir))
         (default-config (json-read-file (expand-file-name "lsp-bridge/langserver/pyright.json" user-emacs-directory)))
         (settings (plist-get default-config :settings))
         )
    (plist-put settings :pythonPath (executable-find "python"))
    (make-directory (file-name-directory custom-config) t)
    (with-temp-file custom-config (insert (json-encode default-config)))
    custom-config))

(add-hook 'python-mode-hook
          (lambda ()
            (setq-local lsp-bridge-get-single-lang-server-by-project
                        'local/lsp-bridge-get-single-lang-server-by-project)))

(add-hook 'pyvenv-post-activate-hooks
          (lambda ()
            (lsp-bridge-restart-process)))


(provide 'init-lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
