;;; clasker-github.el --- Github backend for Clasker

;; Copyright (C) 2012  David Vázquez
;; Copyright (C) 2012  Raimon Grau

;; Author: David Vázquez <davazp@gmail.com>
;;         Raimon Grau <raimonster@gmail.com>
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

;;; Add support for github issues. It adds new github isses as tickets,
;;; and if you use magit, it prepends the number of github-issue in the
;;; commit log buffer.
;;;
;;; The provided functionality is accessed through the function
;;; `clasker-github-import-from-github'. it imports issues from a
;;; github repo. creating new clasker tickets or updating tickets that
;;; were already in the ticket database.

;;; Tickets fetched from github answer t to the method
;;; `clasker-github-ticket-p'.
;;;

;;; Properties added:
;;; gh-source => user/repo of the ticket
;;; gh-id => Number of the issue in github repo
;;;

;;; Magit integration is minimally supported adding the prefix [#xxx]
;;; on commit messages where xxx is the github id of
;;; `clasker-active-ticket'.

;;

;;; Code:

;(require 'clasker)
(require 'gh-issues)
(require 'gh-auth)

(defvar clasker-github-remote nil "Enable modifying remote content
of gh-issues. when non-nil, changing the description of a ticket
in clasker, or archiving it will modify remote repo" )

(defclass clasker-github-ticket (clasker-ticket) ()
  "Subclass for clasker-tickets that belong to a github repository issue")

(defun clasker-iso8601-timestring (string)
  (save-match-data
    (string-match "^\\([[:digit:]]\\{4\\}\\)-\\([[:digit:]]\\{2\\}\\)-\\([[:digit:]]\\{2\\}\\)T\\([[:digit:]]\\{2\\}\\):\\([[:digit:]]\\{2\\}\\):\\([[:digit:]]\\{2\\}\\)Z$" string)
    (encode-time
     (string-to-number (match-string 6 string))
     (string-to-number (match-string 5 string))
     (string-to-number (match-string 4 string))
     (string-to-number (match-string 3 string))
     (string-to-number (match-string 2 string))
     (string-to-number (match-string 1 string))
     t)))

(defmethod clasker-ticket-headline ((ticket clasker-github-ticket))
  (concat (format "[#%s]:" (clasker-ticket-get-property ticket 'gh-id))
          (call-next-method)))

(defun clasker-github-issue-to-ticket (issue source)
  "Creates a clasker ticket from an instance of gh-issue from
gh.el. Adding the extra properties needed for clasker."
  (let ((ticket (clasker-allocate-ticket 'clasker-github-ticket)))
    (clasker-ticket-set-property
     ticket
     'description
     (concat (oref issue title) "\n"
             (replace-regexp-in-string "" ""
                                       (or
                                        (oref issue body)
                                        ""))))
    (clasker-ticket-set-property ticket 'timestamp
                                 (and (oref issue created_at)
                                      (clasker-iso8601-timestring  (oref issue created_at))))
    (clasker-ticket-set-property ticket 'gh-id (oref issue number))
    (clasker-ticket-set-property ticket 'gh-source source)
    ticket))

(defmethod slot-unbound ((issue gh-issues-issue) class name fn) "")

(defun clasker-github-import-from-github (source)
  "Import a list of issues from a user/project in github."
  (interactive "MRepository (user/project): ")
  (let* ((gh-api (gh-issues-api3))
         (source source)
         (user/project source)
         (response (apply #'gh-issues-issue-list gh-api (split-string source "/"))))
    (gh-api-add-response-callback response
                                  (lambda (issues)
                                   (clasker-github-save-tickets issues source)
                                   ;(clasker-github-save-tickets issues "kidd/readerly")
                                    ))))

(defun clasker--github-tickets ()
  "returns a hash table with all clasker-github-tickets. The keys
  are strings \"user/repo/id\" and the tickets as values"
  (let ((table (make-hash-table :test #'equal))
        (tickets (remove-if-not 'clasker-github-ticket-p (clasker-load-tickets))))
    (dolist (ticket tickets)
      (puthash (concatenate 'string (clasker-ticket-get-property ticket 'gh-source) "/"
                            (number-to-string
                             (clasker-ticket-get-property ticket 'gh-id))) ticket table))
    table))

(defun clasker-github-save-tickets (issues source)
  "Saves all tickets that came in the same reply as clasker
tickets. Creating new tickets or updating content in old ones.
ALERT!: if there are [nil]: tickets. this function fails miserably bc
clasker--github-tickets fails miserably"
  (interactive)
  (let ((gh-tickets (clasker--github-tickets)))
    (dolist (issue issues)
      (let* (
             (clasker-ticket (gethash (concatenate 'string source "/"
                                                   (number-to-string (oref issue number)))
                                      gh-tickets)))


        (if clasker-ticket
            (progn
              (clasker-ticket-set-property clasker-ticket 'description
                                           (concat (oref issue title) "\n"
                                                   (replace-regexp-in-string "" ""
                                                                             (or
                                                                              (oref issue body)
                                                                              ""))))
              (clasker-save-ticket clasker-ticket))
          (clasker-save-ticket (clasker-github-issue-to-ticket issue source))))))
  (clasker-revert))

;;; TODO: NIY
(defmethod clasker-github-save-ticket-to-github ((ticket clasker-github-ticket)))

(defun gh-issues-api3 (&optional sync auth)
  (gh-issues-api "api" :sync sync :cache nil
                 :auth (make-instance 'gh-oauth-authenticator)
                 :num-retries 1))

(defun clasker-github-update-ticket (ticket other)
  "Updates the description of ticket with value of other's
description. Add more fields here if you care about more data."
  (clasker-ticket-set-property ticket 'description
                               (clasker-ticket-get-property other 'description))
  ticket)

;;; Archive is synchronized with open/closed state
;; (clasker-add-property-hook 'archived 'clasker-github-archive)
;; (defmethod clasker-github-archive ((ticket clasker-github-ticket) _property new-value)
;;   (let ((ticket-source (split-string (clasker-ticket-get-property ticket 'gh-source) "/"))
;;         (id (clasker-ticket-get-property ticket 'gh-id)))
;;    (gh-issues-issue-update (gh-issues-api3)
;;                            (car ticket-source)
;;                            (cadr ticket-source)
;;                            (number-to-string id)
;;                            `(("state" . ,(if new-value "closed" "open"))))))

;;; Experimental Magit support

(when (featurep 'magit)
 (defun clasker-github-magit-log-edit-append (str)
   (with-current-buffer (get-buffer-create magit-log-edit-buffer-name)
     (goto-char (point-min))
     (insert str)))

 (defun clasker-github-set-description-on-magit-commit ()
   (when clasker-active-ticket
     (let ((gh-id (clasker-ticket-get-property
                       clasker-active-ticket 'gh-id) ))
       (if gh-id
           (clasker-github-magit-log-edit-append
            (concat "[#" (number-to-string
                          gh-id) "]"))))))

 (add-hook 'magit-log-edit-mode-hook 'clasker-github-set-description-on-magit-commit))

(provide 'clasker-github)

;;; Local variables:
;;; lexical-binding: t
;;; indent-tabs-mode: nil
;;; fill-column: 80
;;; End:

;;; clasker-github.el ends here
