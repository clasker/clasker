;;; clasker-debug.el --- developer tools

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

;;

;;; Code:

(defvar clasker-debug-info 'description "property that is shown in tooltip")

(defun clasker-debug-tooltips ()
  (interactive)
  (with-current-buffer "*Clasker*"
    (let ((inhibit-read-only t))
      (beginning-of-buffer)
      (search-forward "\n\n")
      (while (not (equal (get-text-property (point) 'clasker-ticket)
                         (get-text-property (progn
                                              (clasker-next-ticket) (point))
                                            'clasker-ticket )))
        (put-text-property (point) (clasker-end-of-ticket) 'help-echo
                           (clasker-ticket-get-property
                            (get-text-property (point) 'clasker-ticket)
                            clasker-debug-info))))))

(tooltip-mode 1)
(add-hook 'clasker-display-hook 'clasker-debug-tooltips)

(provide 'clasker-debug)
-;;; clasker-debug.el ends here
