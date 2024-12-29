;; init-ui.el --- UI settings -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Theme
(use-package circadian
  :hook
  (emacs-startup . circadian-setup)
  :config
  (setq calendar-latitude 40.0)
  (setq calendar-longitude 116.4)
  (setq circadian-themes '((:sunrise . (
					;; modus-operandi
					adwaita
					))
                           (:sunset  . (
					modus-vivendi
					))
			   )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tab Bar
(use-package tab-bar
  :hook
  (window-setup . tab-bar-mode)
  :custom
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-close-button-show nil)
  (tab-bar-tab-hints t)
  (tab-bar-separator "")
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-tab-name-truncated-max 20)
  (tab-bar-auto-width nil)
  :config
  (customize-set-variable 'tab-bar-select-tab-modifiers '(super))
  (setq tab-bar-tab-name-format-function
        (lambda (tab i)
          (let ((face (funcall tab-bar-tab-face-function tab)))
            (concat
             (propertize " " 'face face)
             (propertize (number-to-string i) 'face `(:inherit ,face :weight ultra-bold :underline t))
             (propertize (concat " " (alist-get 'name tab) " ") 'face face)))))
  (setq tab-bar-tab-name-function
        (lambda () (let* ((raw-tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
                     (count (length (window-list-1 nil 'nomini)))
                     (truncated-tab-name (if (< (length raw-tab-name)
                                                tab-bar-tab-name-truncated-max)
                                             raw-tab-name
                                           (truncate-string-to-width raw-tab-name
                                                                     tab-bar-tab-name-truncated-max
                                                                     nil nil tab-bar-tab-name-ellipsis))))
                (if (> count 1)
                    (concat truncated-tab-name "(" (number-to-string count) ")")
                  truncated-tab-name))))
  (when (daemonp)
    (add-hook 'after-make-frame-functions
              #'(lambda (&rest _) (force-mode-line-update))))
  )

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
;;; Modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(provide 'init-ui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
