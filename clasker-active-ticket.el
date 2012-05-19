;;; clasker-active-ticket.el --- managing active clasker task

;; Copyright (C) 2012  Raimon Grau

;; Author: Raimon Grau <raimonster@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Clasker-active-ticket uses a simple mechanism to enable an active
;;; ticket.  That should be used to keep track of which task you're
;;; doing in any time.  It's not very useful per se, but this sets the
;;; ground for further add-ons.

;;; clasker-active-ticket sets 2 hooks.
;;;
;;; `clasker-activate-ticket-hook' is triggered when a task is about
;;; to be set as active. `clasker-deactivate-ticket-hook' is
;;; triggered when a task is about to be unset as active
;;;
;;;

;;; (add-hook 'clasker-activate-ticket-hook 'foo)
;;; (add-hook 'clasker-deactivate-ticket-hook 'foo)

;;; Code:

(defvar clasker-active-ticket nil "current active ticket")
(defvar clasker-activate-ticket-hook nil "hook on activating a task")
(defvar clasker-deactivate-ticket-hook nil "hook on deactivating a task")


(defmethod clasker-active-p ((ticket clasker-ticket))
  "answers t if ticket is the current active ticket"
  (equal (slot-value clasker-active-ticket 'line) (slot-value ticket 'line)))

(defun clasker-deactivate-ticket (ticket)
  "removes active property from ticket and runs
`clasker-deactivate-ticket-hook'"
  (run-hooks 'clasker-deactivate-ticket-hook)

  (when (clasker-active-p ticket)
    (setq clasker-active-ticket nil))

  (clasker-ticket-delete-property ticket 'active)
  (clasker-save-ticket ticket))

(defun clasker-activate-ticket (ticket)
  "activates ticket and runs `clasker-activate-ticket-hook'"
  (when (and clasker-active-ticket
             (not  (eq clasker-active-ticket ticket)))
    (clasker-deactivate-ticket clasker-active-ticket))

  (clasker-ticket-set-property ticket 'active t)
  (setq clasker-active-ticket ticket)
  (clasker-save-ticket ticket)
  (run-hooks 'clasker-activate-ticket-hook))

(defun clasker-highlight-active-ticket ()
  "highlight active buffer if any. Intended to be used in
`clasker-display-hook'"
  (interactive)
  (when clasker-active-ticket
    (with-current-buffer clasker-buffer-name
      (let ((inhibit-read-only t))
        (beginning-of-buffer)
        (search-forward "\n\n")
        (while (and
                (not (eq (point) (point-max)))
                (not (clasker-active-p (get-text-property (point) 'clasker-ticket))))
          (clasker-next-ticket))
        (put-text-property (point) (clasker-end-of-ticket) 'font-lock-face 'bold)))))

(defun clasker-pomodoro-initialize ()
  (add-hook 'clasker-display-hook 'clasker-highlight-active-ticket)
  (add-to-list
   'clasker-default-actions
   '("Activate" . clasker-activate-ticket )))

(clasker-pomodoro-initialize)

(provide 'clasker-active-ticket)
;;; clasker-active-ticket.el ends here
