;; init-font.el --- Fonts settings -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun load-font-setup ()
  (let* ((emacs-font-size 25)
         (chinese-font-name "TsangerJinKai03-6763")
         (english-font-name "Sarasa Term SC Nerd")
         (font-spec-english (font-spec :family english-font-name :size emacs-font-size))
         (font-spec-chinese (font-spec :family chinese-font-name :size emacs-font-size)))
    (when (display-grayscale-p)
      (set-frame-font font-spec-english)
      (set-fontset-font (frame-parameter nil 'font) 'unicode font-spec-english)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font) charset font-spec-chinese)))
    ))

(load-font-setup)

;; This is hacking to fix Emacs 29 will decrease font after standby.
(add-function :after after-focus-change-function #'load-font-setup)


(dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'haskell-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'maxima-mode-hook
               'ielm-mode-hook
               'sh-mode-hook
               'makefile-gmake-mode-hook
               'php-mode-hook
               'python-mode-hook
               'js-mode-hook
               'go-mode-hook
               'qml-mode-hook
               'jade-mode-hook
               'css-mode-hook
               'ruby-mode-hook
               'coffee-mode-hook
               'rust-mode-hook
               'qmake-mode-hook
               'lua-mode-hook
               'swift-mode-hook
               'web-mode-hook
               'markdown-ts-mode-hook
               'llvm-mode-hook
               'conf-toml-mode-hook
               'nim-mode-hook
               'typescript-mode-hook
               ))
  (add-hook hook #'(lambda () (load-font-setup))))

(provide 'init-font)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-font.el ends here
