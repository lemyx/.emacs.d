;;; init-tex.el --- TeX settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; cdlatex
(use-package cdlatex
  :after tex)

;; TeX Core
(use-package tex
  :ensure auctex
  :demand t
  :custom
  ; 自动解析新文件 (usepackage, bibliograph, newtheorem) 等信息
  (TeX-parse-selt t)
  (TeX-PDF-mode t)
  ; 正反向搜索
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method 'synctex)
  ; 使用 pdf-tools 预览
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  :config
  (add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode))
  (setq TeX-auto-save t)
  (setq TeX-save-query nil)
  (setq TeX-show-compilation t)
  ; 编译时问询主文件名称
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (cdlatex-mode)
                               (reftex-mode)
                               (prettify-symbols-mode t)
                               (outline-minor-mode)
                               (outline-hide-body)
                               (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
                               )))

(provide 'init-tex)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tex.el ends here
