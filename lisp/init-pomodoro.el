;;; init-pomodoro.el --- Pomodoro settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-pomodoro
(use-package org-pomodoro
  :custom-face
  (org-pomodoro-mode-line ((t (:inherit warning))))
  (org-pomodoro-mode-line-overtime ((t (:inherit error))))
  (org-pomodoro-mode-line-break ((t (:inherit success))))
  :bind (:map org-mode-map
              ("C-c C-x m" . org-pomodoro))
  :init
  (with-eval-after-load 'org-agenda
    (bind-keys :map org-agenda-mode-map
               ("K" . org-pomodoro)
               ("C-c C-x m" . org-pomodoro))))

;; https://github.com/devbins/.emacs.d/blob/5341a41e2b100c1228eb2438aa5a83927857cfb0/lisp/init-func.el#L335
(defun notify-osx (title msg)
  (call-process "terminal-notifier"
                nil 0 nil
                "-group" "Emacs"
                "-title" title
                ;; "-sender" "org.gnu.Emacs"
                "-message" msg
		"-sound" "Glass"
                "-active" "org.gnu.Emacs"))
;; org-pomodoro mode hooks
(add-hook 'org-pomodoro-started-hook
	  (lambda ()
	    (notify-osx "Pomodoro started!" "Time for a pomodoro.")))

(add-hook 'org-pomodoro-finished-hook
          (lambda ()
            (notify-osx "Pomodoro completed!" "Time for a break.")))

(add-hook 'org-pomodoro-break-finished-hook
          (lambda ()
            (notify-osx "Pomodoro Short Break Finished" "Ready for Another?")))

(add-hook 'org-pomodoro-long-break-finished-hook
          (lambda ()
            (notify-osx "Pomodoro Long Break Finished" "Ready for Another?")))

(add-hook 'org-pomodoro-killed-hook
          (lambda ()
            (notify-osx "Pomodoro Killed" "One does not simply kill a pomodoro!")))

(provide 'init-pomodoro)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-pomodoro.el ends here
