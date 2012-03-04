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

;;

;;; Code:

(defvar clasker-active-ticket nil "current active ticket")
(defvar clasker-activate-ticket-hook nil "hook on activating a task")
(defvar clasker-deactivate-ticket-hook nil "hook on deactivating a task")

;; (add-hook 'clasker-activate-ticket-hook 'foo)
;; (add-hook 'clasker-deactivate-ticket-hook 'foo)

(defmethod clasker-active-p ((ticket clasker-ticket))
  (equal clasker-active-ticket ticket))

(defun clasker-deactivate-ticket (ticket)
  (run-hooks 'clasker-deactivate-ticket-hook)
  (clasker-ticket-set-property ticket 'active nil))

(defun clasker-activate-ticket (ticket)
  (when (and clasker-active-ticket
             (not  (eq clasker-active-ticket ticket)))
    (clasker-deactivate-ticket clasker-active-ticket))

  (clasker-ticket-set-property ticket 'active t)
  (setq clasker-active-ticket ticket)
  (clasker-save-ticket ticket)
  (run-hooks 'clasker-activate-ticket-hook))

(defun clasker-pomodoro-initialize ()
  (push
   '("Activate" . clasker-activate-ticket )
   clasker-default-actions))

(clasker-pomodoro-initialize)

(provide 'clasker-active-ticket)
;;; clasker-active-ticket.el ends here
