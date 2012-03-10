;;; clasker-github.el --- Github backend for Clasker

;; Copyright (C) 2012  David Vázquez

;; Author: David Vázquez <davazp@gmail.com>
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

(require 'clasker)
(require 'gh-issues)

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

(defun clasker-github-issue-to-ticket (issue)
  (let ((ticket (make-instance 'clasker-ticket)))
    (clasker-ticket-set-property ticket 'description
                                 (concat (oref issue title) "\n" (replace-regexp-in-string "" "" (oref issue body))))
    (clasker-ticket-set-property ticket 'timestamp
                                 (and (oref issue created_at)
                                      (clasker-iso8601-timestring  (oref issue created_at))))
    ticket))

(defun clasker-import-from-github (source)
  "Import a list of issues from a user/project in github."
  (interactive "MRepository (user/project): ")
  (let* ((gh-api (gh-issues-api2))
         (user/project (split-string source "/"))
         (response (apply #'gh-issues-list gh-api user/project)))
    (gh-api-add-response-callback  response 'clasker-save-github)))

(defun clasker-save-github (issues)
  (interactive)
  (dolist (issue issues)
    (clasker-save-ticket
     (clasker-github-issue-to-ticket issue)))
  (clasker-revert))

(defun hola ()
  (setq issues (gh-issues-list api "kidd" "readerly")))

(provide 'clasker-github)
;;; clasker-github.el ends here
