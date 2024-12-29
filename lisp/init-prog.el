;;; init-prog.el --- Programming settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-revert
(global-auto-revert-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fluently Edit Large Files
;; https://emacs-china.org/t/topic/25811/6
(setq-default bidi-display-reordering nil)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Line Numbers
(use-package display-line-numbers
      :ensure nil
      :init
      (setq display-line-numbers-width-start t)
      (setq-default display-line-numbers t)
      :config
      (dolist (mode '(org-mode-hook
                      term-mode-hook
                      eshell-mode-hook))
        (add-hook mode (lambda () (display-line-numbers-mode 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; indent-bars
(use-package indent-bars
  ; reference: https://elpa.gnu.org/devel/doc/use-package.html
  :vc (indent-bars :url "https://github.com/jdtsmith/indent-bars.git"
		   :rev :newest)
  :hook (prog-mode . indent-bars-mode)
  :config
  (setq indent-bars-display-on-blank-lines t)
  (setq indent-bars-width-frac 0.2)
  (setq indent-bars-color '(highlight :face-bg t :blend 0.2))
  (setq indent-bars-zigzag nil)
  (setq indent-bars-highlight-current-depth nil)
  (setq indent-bars-prefer-character t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init-prog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
