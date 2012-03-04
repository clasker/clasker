(defvar clasker-active-ticket nil "current active ticket")
(defvar clasker-activate-ticket-hook nil "hook on activating a task")
(defvar clasker-deactivate-ticket-hook nil "hook on deactivating a task")

;; (add-hook 'clasker-activate-ticket-hook 'foo)
;; (add-hook 'clasker-deactivate-ticket-hook 'foo)

(defmethod clasker-active-p ((ticket clasker-ticket))
  (eq clasker-active-ticket ticket))

(defun clasker-deactivate-ticket (ticket)
  (run-hooks 'clasker-deactivate-ticket-hook)
  (clasker-ticket-set-property ticket '(active . nil)))

(defun clasker-activate-ticket (ticket)
  (when (and clasker-active-ticket
             (not  (eq clasker-active-ticket ticket)))
    (clasker-deactivate-ticket clasker-active-ticket))

  (clasker-ticket-set-property ticket '(active . t))
  (setq clasker-active-ticket ticket)
  (run-hooks 'clasker-activate-ticket-hook))

(defun clasker-pomodoro-initialize ()
  (push
   '("Activate" . clasker-activate-ticket )
   clasker-default-actions))

;(clasker-pomodoro-initialize)
(provide 'clasker-active-ticket)
