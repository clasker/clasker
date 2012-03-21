;;; clasker-pomodoro.el --- Basic pomodoro technique on clasker

;; Copyright (C) 2012  Raimon Grau

;; Author: Raimon Grau <raimonster@gmail.com>
;; Keywords: tools

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

;;; Implementation of pomodoro technique workflow. This plugin depends
;;; on `clasker-active-ticket', and adds pomodoros to the active tasks
;;; at every moment. No reporting is done atm but info of pomodoros
;;; spent in each task is stored in the property
;;; `clasker-pomodoro-count'.
;;;
;;; Modeline is modified to show the minutes of the current item.
;;;
;;; There are some hooks where you can attach your functions.
;;; `clasker-pomodoro-start-pomodoro-hook' is called just before
;;; taking any action when starting a pomodoro and
;;; `clasker-pomodoro-end-pomodoro-hook' when ending any pomodoro.
;;;

;;; Code:

(defvar clasker-pomodoro-items '(("Work" . 25) ("Play" . 5) ("Long play" . 15))
  "List of definitions of different types of time-slots (name . minutes) ")

(defvar clasker-pomodoro-working-item (caar clasker-pomodoro-items)
  "after which time slot you consider a pomodoro finished")

(defvar clasker-pomodoro-pattern   (let ((a (mapcar (lambda (x) (assoc x clasker-pomodoro-items))
                                                    '("Work" "Play" "Work" "Play" "Work" "Play" "Work" "Long play"))))
                                     (nconc a a))
  "Ring with the complete pattern of the workflow")

(defvar clasker-pomodoro-timer nil)
(defvar clasker-pomodoro-current-state 0 "Minutes on current time slot")

(defun clasker-pomodoro-start ()
  (interactive)
  (clasker-pomodoro-start-pomodoro)
  (setq clasker-pomodoro-timer (run-at-time nil 60 'clasker-pomodoro-update)))

(defun clasker-pomodoro-update ()
  (incf clasker-pomodoro-current-state)
  (when (>=  clasker-pomodoro-current-state (cdar clasker-pomodoro-pattern))
    (clasker-pomodoro-end-pomodoro)
    (pop clasker-pomodoro-pattern)
    (clasker-pomodoro-start-pomodoro))
  (clasker-pomodoro-update-modeline))

(defun clasker-pomodoro-update-modeline ()
  (setq global-mode-string (format "[%s:%d]" (caar clasker-pomodoro-pattern)
                                   clasker-pomodoro-current-state)))

(defun clasker-pomodoro-start-pomodoro ()
  (run-hooks 'clasker-pomodoro-start-pomodoro-hook)
  (setq clasker-pomodoro-current-state 0))

(defun clasker-pomodoro-end-pomodoro ()
  (interactive)
  (run-hooks 'clasker-pomodoro-end-pomodoro-hook)
  (when (and clasker-active-ticket (string=
                                    (caar clasker-pomodoro-pattern)
                                    clasker-pomodoro-working-item))  ; HACK
    (setf counter (1+ (or (clasker-ticket-get-property clasker-active-ticket 'clasker-pomodoro-count)
                          0)))
    (clasker-ticket-set-property clasker-active-ticket 'clasker-pomodoro-count counter)
    (clasker-save-ticket clasker-active-ticket)))

(provide 'clasker-pomodoro)
;;; clasker-pomodoro.el ends here
