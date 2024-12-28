;;; init-env.el --- Environment settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package exec-path-from-shell
  :demand t
  :config
  (exec-path-from-shell-initialize))

(provide 'init-env)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-env.el ends here
