;;; init-session.el --- Session settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; desktop
(setq desktop-load-locked-desktop t)          ; don't popup dialog ask user, load anyway
(setq desktop-restore-frames t)               ; save and restore the frame and window configuration
(setq desktop-restore-in-current-display nil) ; restore frames in current display
(setq desktop-restore-forces-onscreen "all")  ; restore frames that are partially offscreen onscreen

(defun emacs-session-restore ()
  "Restore emacs session."
  (interactive)
  (ignore-errors
    ;; Restore session.
    (desktop-read "~/.emacs.d/")
    ))

(defun emacs-session-save (&optional arg)
  "Save emacs session."
  (interactive "p")
  (ignore-errors
    (if (equal arg 4)
        ;; Kill all buffers if with prefix argument.
        (mapc 'kill-buffer (buffer-list))
      ;; Save all buffers before exit.
      (auto-save-buffers))
    ;; Save session.
    (make-directory "~/.emacs.d/" t)
    (desktop-save "~/.emacs.d/")
    ;; Exit emacs.
    (kill-emacs)))

(provide 'init-session)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-session.el ends here
