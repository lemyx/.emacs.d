;;; init-pdf.el --- pdf settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode) ; pdf 文件默认打开方式
  :bind
  (:map pdf-view-mode-map
   ("d" . pdf-view-next-page-command)
   ("a" . pdf-view-previous-page-command)
   ("s" . pdf-view-scroll-up-or-next-page)
   ("w" . pdf-view-scroll-down-or-previous-page)
   :map pdf-history-minor-mode-map
   ("b" . pdf-history-backward)
   :map pdf-annot-minor-mode-map
   ("C-a a" . pdf-annot-add-highlight-markup-annotation)
   ("C-a s" . pdf-annot-add-squiggly-markup-annotation)
   ("C-a u" . pdf-annot-add-underline-markup-annotation)
   ("C-a d" . pdf-annot-delete))
  :custom
  (pdf-view-midnight-colors '("#000000" . "#9bCD9b"))            ; 夜间模式设置绿色底色
  :config
  (pdf-tools-install)
  (setq native-comp-deferred-compilation-deny-list '(".*pdf.*"))
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))  ; 设置 pdf-tools 打开 pdf
  (require 'pdf-annot)                                           ; 设置 pdf-annot-mimor-mode-map
  (require 'pdf-history)                                         ; 设置 pdf-history-minor-mode-map
  (add-hook 'pdf-view-mode-hook 'pdf-view-fit-width-to-window)   ; 默认适应页宽
  ; (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode) ; 默认夜间模式
  (add-hook 'TeX-after-compilation-finished-functions
	    #'TeX-revert-document-buffer) ; 完成编译后刷新 pdf 文件
  )

(provide 'init-pdf)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-pdf.el ends here
