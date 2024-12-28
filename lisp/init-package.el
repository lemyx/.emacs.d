;;; init-package.el --- The main init entry for Emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; configure package archives
(require 'package)
(setq package-check-signature nil)
(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu"          . "https://elpa.gnu.org/packages/")
	("nongnu"       . "https://elpa.nongnu.org/nongnu/")))
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; configure use-package
(require 'use-package)
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t))

;; configure vc-use-package
; As of 2023-05-16, vc-use-package has been merged into Emacs master! Thus, if you are using a recent enough version of Emacs 30+, you don't need to install this package and can instead use use-packages own :vc keyword.
; Note that the keyword syntax differs in the core differs from that used in this package! Instead of a MELPA-style :vc (:fetcher github :repo oantolin/math-delimiters), the built-in keyword uses ELPA-style :vc (:url "https://github.com/oantolin/math-delimiters") package specifications. 
(when (< emacs-major-version 30)
  (unless (package-installed-p 'vc-use-package)
    (package-vc-install "https://github.com/slotThe/vc-use-package"))
  (require 'vc-use-package))

(provide 'init-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
