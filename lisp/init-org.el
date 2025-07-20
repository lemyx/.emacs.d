;;; init-org.el --- Org settings -*- lexical-binding: t -*-;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org
(use-package org
  :config
  ; Line Truncation @ https://www.gnu.org/software/emacs/manual/html_node/emacs/Line-Truncation.html
  (add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
  ;; ; Dynamic Headline Numbering @ https://orgmode.org/manual/Dynamic-Headline-Numbering.html
  ;; (setq org-startup-numerated t)
  ; Hide emphasis markup characters in buffers @ https://orgmode.org/manual/Emphasis-and-Monospace.html
  (setq org-hide-emphasis-markers t)
  ; if there is a #+ATTR.*: width="200", resize to 200, otherwise resize to 320
  (setq org-image-actual-width '(320))
  (setq org-startup-with-inline-images t)
  ; https://orgmode.org/manual/Fast-access-to-TODO-states.html
  ; https://orgmode.org/guide/Progress-Logging.html
  (setq org-todo-keywords
	'((sequence "TODO(t)" "HOLD(h!)" "WIP(i!)" "WAIT(w!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
	  (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f!)")))
  ; https://orgmode.org/manual/Faces-for-TODO-keywords.html
  (setq org-todo-keyword-faces
	'(("TODO"       . (:foreground "#7c7c75" :weight bold))
	  ("HOLD"       . (:foreground "#feb24c" :weight bold))
          ("WIP"        . (:foreground "#0098dd" :weight bold))
          ("WAIT"       . (:foreground "#9f7efe" :weight bold))
          ("DONE"       . (:foreground "#50a14f" :weight bold))
          ("CANCELLED"  . (:foreground "#ff6480" :weight bold))
          ("REPORT"     . (:foreground "magenta" :weight bold))
          ("BUG"        . (:foreground "red"     :weight bold))
          ("KNOWNCAUSE" . (:foreground "yellow"  :weight bold))
          ("FIXED"      . (:foreground "green"   :weight bold))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-modern
(use-package org-modern
  :hook (after-init . (lambda ()
                        (setq org-modern-hide-stars 'leading)
                        (global-org-modern-mode t)))
  :config
  (setq org-modern-table nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-appear
(use-package org-appear
  :hook
  (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers t)
  (setq org-appear-autoentities t)
  (setq org-appear-autokeywords t)
  (setq org-appear-inside-latex t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-bars
(use-package org-bars
  :vc (org-bars :url "https://github.com/tonyaldon/org-bars.git"
		:rev :newest)
  :hook (org-mode . org-bars-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ox-gfm
;; org-mode 导出为 markdown @ https://emacs-china.org/t/org-mode-markdown/17393/3
;; 如果将 #+begin_src #+end_src 紧跟标题, 则导出效果正常 (workaround)
(use-package ox-gfm
  :after org
  :config
  (setq org-export-with-sub-superscripts nil)
  (setq org-export-headline-levels 5)
  (setq org-export-coding-system 'utf-8)
  (setq org-export-with-broken-links 'mark)
  (setq org-link-file-path-type 'relative)

  ;; 定义自动导出为 Markdown 的函数
  (defun export-org-to-markdown-if-org-mode ()
    "Export current org-mode buffer to a markdown file if in org-mode."
    (when (eq major-mode 'org-mode) ; 确保当前缓冲区是 org-mode
      (org-gfm-export-to-markdown)))

  ;; 添加到 org-mode 的保存钩子中
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'after-save-hook 'export-org-to-markdown-if-org-mode nil 'make-it-local))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-download
(use-package org-download
  :ensure t
  :config
  (setq-default org-download-heading-lvl nil)
  (setq-default org-download-image-dir "./img")
  :hook
  (org-mode . org-download-enable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; valign
(use-package valign
  :config
  (add-hook 'org-mode-hook #'valign-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-org-md
(use-package auto-org-md)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; toc-org
(use-package toc-org
  :config
  (add-hook 'org-mode-hook 'toc-org-mode))

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
