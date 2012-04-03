;;; clasker-external.el --- 

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

(defun clasker-import-external-tickets ()
  (interactive)
  (dolist (file (directory-files "~/.clasker.d/export/" t "clasker-*"))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (ignore-errors
        (let* ((raw-props (read-from-whole-string (buffer-string)))
               (props (mapcar (lambda (prop)
                                (cons (car prop)
                                      (if (stringp (cdr prop))
                                          (clasker--unquote-string (cdr prop))
                                        (cdr prop))))
                              raw-props))
               (ticket (make-instance 'clasker-ticket)))
          (oset ticket properties props)
          (clasker-save-ticket ticket)
          (delete-file file))))))

(add-hook 'clasker-mode-hook 'clasker-import-external-tickets)

(provide 'clasker-external)
;;; clasker-external.el ends here
