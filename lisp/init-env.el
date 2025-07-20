;;; init-env.el --- Environment settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; (use-package exec-path-from-shell
;;   :demand t
;;   :config
;;   (exec-path-from-shell-initialize))

;; https://emacs-china.org/t/exec-path-from-shell/2515/14?u=ententent
;; 复用exec-path-from-shell内的一个方法，其实自己实现也可以
(defun exec-path-from-shell-setenv (name value)
  "Set the value of environment var NAME to VALUE.
Additionally, if NAME is \"PATH\" then also set corresponding
variables such as `exec-path'."
  (setenv name value)
  (when (string-equal "PATH" name)
    (setq eshell-path-env value
          exec-path (append (parse-colon-path value) (list exec-directory)))))
;;调用exec-path-from-shell-setenv，手动传入参数，每次修改PATH都要到这里修改一下。还好我修改不频繁
(exec-path-from-shell-setenv "PATH" "/Users/xiudi/.catpaw/bin:/usr/local/Caskroom/miniconda/base/bin:/usr/local/bin:/usr/local/sbin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin:/Library/Apple/usr/bin:/Library/TeX/texbin:/Users/xiudi/.cargo/bin:/Applications/iTerm.app/Contents/Resources/utilities")

(provide 'init-env)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-env.el ends here
