;;; init-tex.el --- TeX settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Ensure auctex is installed
(unless (package-installed-p 'auctex)
  (package-refresh-contents)
  (package-install 'auctex))

(require 'tex)

(use-package cdlatex
  :ensure t
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; outline-minor-mode
;; https://www.bilibili.com/video/BV1i4411a74k?vd_source=d2fb836a26e496b507775f8784008115&spm_id_from=333.788.videopod.sections
(use-package outline
  :init
  (setq outline-minor-mode-prefix (kbd "C-'"))
  :bind
  (:map outline-minor-mode-map
        ("C-' t" . outline-hide-body)
        ("C-' e" . outline-show-entry)
        ("C-' p" . outline-backward-same-level)
        ("C-' n" . outline-forward-same-level)
        ("C-' a" . outline-show-all))
  :hook
  (LaTeX-mode . (lambda ()
		  (outline-minor-mode)
		  (outline-hide-body)))
  )

(setq-default TeX-master nil)
(setq TeX-global-PDF-mode t TeX-engine 'xetex)
(add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
(setq TeX-command-default "XeLaTeX")
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-method 'synctex)
(setq TeX-view-program-selection '((output-pdf "PDF Tools")))

(provide 'init-tex)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tex.el ends here
