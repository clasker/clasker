;;; clasker-workflows.el --- Simple workflow for activating/deactivating tickets
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

(defun clasker-action-WORKFLOWS (ticket)
  (case (clasker-ticket-get-property ticket 'status)
    (todo
     '(("Start" . (lambda (ticket) (clasker-ticket-set-property ticket 'status 'started)))
       ("Cancel" . (lambda (ticket) (clasker-ticket-set-property ticket 'status 'cancelled)))))
    (cancelled
     '(("Reopen" . (lambda (ticket) (clasker-ticket-set-property ticket 'status 'todo)))))
    (started
     '(("Complete" . (lambda (ticket) (clasker-ticket-set-property ticket 'status 'done)))
       ("Cancel" . (lambda (ticket) (clasker-ticket-set-property ticket 'status 'cancelled)))))
    (done
     '(("Reopen" . (lambda (ticket) (clasker-ticket-set-property ticket 'status 'started)))))
    (t
     '(("Accept"  . (lambda (ticket) (clasker-ticket-set-property ticket 'status 'todo)))
       ("Discard" . (lambda (ticket) (clasker-ticket-set-property ticket 'status 'discard)))))))


(defadvice clasker-ticket-headline (after workflow-headline (ticket))
  (setq ad-return-value
        (concat
         (upcase (symbol-name (or (clasker-ticket-get-property ticket 'status) 'new)))
         " "
         ad-return-value)))

(ad-activate 'clasker-ticket-headline)


(provide 'clasker-workflows)
;;; clasker-workflows.el ends here
