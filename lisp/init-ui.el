;; init-ui.el --- UI settings -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Themes

;; ;; Xah Lee
;; ;; initial window settings
;; (setq initial-frame-alist
;;       '((background-color . "honeydew")))
;; ;; ;; subsequent window settings
;; (setq default-frame-alist
;;       '((background-color . "honeydew")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ns-system-appearance-change-functions
;; provided by emacs-plus
;; https://github.com/d12frosted/homebrew-emacs-plus/blob/3e95d573d5f13aba7808193b66312b38a7c66851/patches/emacs-28/system-appearance.patch#L42-L49
;; alternatively, package auto-dark for cross-platform
(defun my/load-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'modus-operandi t))
    ('dark (load-theme 'modus-vivendi t))))

(add-hook 'ns-system-appearance-change-functions #'my/load-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tab Bar
;; (use-package tab-bar
;;   :hook
;;   (window-setup . tab-bar-mode)
;;   :custom
;;   (tab-bar-new-tab-to 'rightmost)
;;   (tab-bar-close-button-show nil)
;;   (tab-bar-tab-hints t)
;;   (tab-bar-separator "")
;;   (tab-bar-new-tab-choice "*scratch*")
;;   (tab-bar-tab-name-truncated-max 20)
;;   (tab-bar-auto-width nil)
;;   :config
;;   (customize-set-variable 'tab-bar-select-tab-modifiers '(super))
;;   (setq tab-bar-tab-name-format-function
;;         (lambda (tab i)
;;           (let ((face (funcall tab-bar-tab-face-function tab)))
;;             (concat
;;              (propertize " " 'face face)
;;              (propertize (number-to-string i) 'face `(:inherit ,face :weight ultra-bold :underline t))
;;              (propertize (concat " " (alist-get 'name tab) " ") 'face face)))))
;;   (setq tab-bar-tab-name-function
;;         (lambda () (let* ((raw-tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
;;                      (count (length (window-list-1 nil 'nomini)))
;;                      (truncated-tab-name (if (< (length raw-tab-name)
;;                                                 tab-bar-tab-name-truncated-max)
;;                                              raw-tab-name
;;                                            (truncate-string-to-width raw-tab-name
;;                                                                      tab-bar-tab-name-truncated-max
;;                                                                      nil nil tab-bar-tab-name-ellipsis))))
;;                 (if (> count 1)
;;                     (concat truncated-tab-name "(" (number-to-string count) ")")
;;                   truncated-tab-name))))
;;   (when (daemonp)
;;     (add-hook 'after-make-frame-functions
;;               #'(lambda (&rest _) (force-mode-line-update))))
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Icons
(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pixel-scroll-precision-mode
;; https://emacs-china.org/t/topic/25114
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-interpolate-page t)
(defun +pixel-scroll-interpolate-down (&optional lines)
  (interactive)
  (if lines
      (pixel-scroll-precision-interpolate (* -1 lines (pixel-line-height)))
    (pixel-scroll-interpolate-down)))

(defun +pixel-scroll-interpolate-up (&optional lines)
  (interactive)
  (if lines
      (pixel-scroll-precision-interpolate (* lines (pixel-line-height))))
  (pixel-scroll-interpolate-up))

(defalias 'scroll-up-command '+pixel-scroll-interpolate-down)
(defalias 'scroll-down-command '+pixel-scroll-interpolate-up)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global-hl-line-mode
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

(provide 'init-ui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
