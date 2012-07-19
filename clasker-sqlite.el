;;; clasker-sqlite.el --- 

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

(require 'pcsv)

(defvar clasker-sqlite-program
  (or (executable-find "sqlite3")
      (error "You need sqlite3 installed in order to use Clasker.")))

(defvar clasker-sqlite-database "~/.clasker.db")

(defun clasker-sql-quote-string (string)
  (concat "'" (replace-regexp-in-string "'" "\\\\'" string) "'"))

(defun clasker-sql-prepare (sql &rest args)
  (apply #'format sql
         (mapcar (lambda (x)
                   (cond
                    ((stringp x)
                     (clasker-sql-quote-string x))
                    ((symbolp x)
                     (clasker-sql-quote-string (symbol-name x)))
                    (t
                     x)))
                 args)))

(defun clasker-sql-nonquery (sql &rest args)
  (let ((file (expand-file-name clasker-sqlite-database)))
    (call-process clasker-sqlite-program nil nil nil  "--csv" file
                  (apply #'clasker-sql-prepare sql args))))

(defun clasker-sql-query (sql &rest args)
  (with-temp-buffer
    (let ((file (expand-file-name clasker-sqlite-database)))
      (call-process clasker-sqlite-program nil t nil  "--csv" file
                    (apply #'clasker-sql-prepare sql args))
      (mapcar (lambda (line)
                (mapcar (lambda (item)
                          (condition-case nil
                              (parse-integer item)
                            (parse-error item)))
                        line))
              (pcsv-parse-buffer)))))

(defun clasker-sql-table-exist-p (name)
  (let ((result (clasker-sql-query "SELECT * FROM sqlite_master WHERE type='table' AND name=%s" name)))
    (and result t)))

(defun clasker-sql-setup-schema ()
  (unless (clasker-sql-table-exist-p "Tickets")
    (clasker-sql-nonquery "
CREATE TABLE IF NOT EXISTS Tickets (
        ID,
        Property,
        Value,
         PRIMARY KEY (ID, Property))")))

(provide 'clasker-sqlite)
;;; clasker-sqlite.el ends here
