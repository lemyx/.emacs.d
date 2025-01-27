;;; init-window.el --- Window settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; winner-mode
(use-package winner-mode
  :ensure nil
  :hook
  (after-init . winner-mode)
  (ediff-quit . winner-undo)
  )

(use-package shackle
  :ensure t
  :hook (after-init . shackle-mode)
  :init
  (setq shackle-lighter "")
  (setq shackle-select-reused-windows nil) ; default nil
  (setq shackle-default-alignment 'below)  ; default below
  (setq shackle-rules
        ;; CONDITION(:regexp)            :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
        '((compilation-mode              :ignore t)
          ("\\*Async Shell.*\\*" :regexp t :ignore t)
          ("\\*corfu.*\\*"       :regexp t :ignore t)
          ("*eshell*"                    :select t                          :size 0.4  :align t     :popup t)
          ("*vterm*"                     :select t                          :size 0.4  :align t     :popup t)
	  ("*DeepSeek*"                  :select t                          :size 0.4  :align t     :popup t)
	  (comint-mode                   :select t                          :size 0.4  :align t     :popup t)
          (helpful-mode                  :select t                          :size 0.6  :align right :popup t)
          ("*Messages*"                  :select t                          :size 0.4  :align t     :popup t)
          ("*Calendar*"                  :select t                          :size 0.3  :align t     :popup t)
          ("*info*"                      :select t                                                  :same t)
          (magit-status-mode             :select t   :inhibit-window-quit t                         :same t)
          (magit-log-mode                :select t   :inhibit-window-quit t                         :same t)
          ))
  )

(use-package popper
  :ensure t
  :bind (("C-`"     . popper-toggle-latest)
         ("M-<tab>" . popper-cycle)
         ("M-\\"    . popper-toggle-type)
         )
  :init
  (setq popper-reference-buffers
        '(
	  "\\*DeepSeek\\*"
	  comint-mode
	  "\\*Messages\\*"
          "\\*Async Shell Command\\*"
          help-mode
          helpful-mode
          occur-mode
          pass-view-mode
          "^\\*eshell.*\\*$" eshell-mode
          "^\\*shell.*\\*$"  shell-mode
	  "^\\*vterm*\\*$"   vterm-mode
          ("\\*corfu\\*" . hide)
          (compilation-mode . hide)
          ;; derived from `fundamental-mode' and fewer than 10 lines will be considered a popup
          (lambda (buf) (with-current-buffer buf
                          (and (derived-mode-p 'fundamental-mode)
                               (< (count-lines (point-min) (point-max))
                                  10))))
          )
        )
  (popper-mode +1)
  (popper-echo-mode +1)
  :config
  ;; group by project.el, projectile, directory or perspective
  (setq popper-group-function nil)

  ;; pop in child frame or not
  (setq popper-display-function #'display-buffer-in-child-frame)

  ;; use `shackle.el' to control popup
  (setq popper-display-control nil)
  )

(use-package ace-window
  :ensure t
  :bind (("C-x o" . 'ace-window)))

(provide 'init-window)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-window.el ends here
