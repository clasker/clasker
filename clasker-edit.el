;;; clasker-edit.el --- Clasker ticket editing

;; Copyright (C) 2012  Raimon Grau
;; Copyright (C) 2012  David Vázquez

;; Authors: David Vázquez <davazp@gmail.com>
;;          Raimon Grau <raimonster@gmail.com>

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

(defun clasker-edit-save-ticket ()
  (interactive)
  (clasker-ticket-set-property current-ticket 'description (buffer-string))
  (clasker-save-tickets)                ;TODO: Remove when loading is
                                        ;not done from file
  (clasker-edit-quit))

(defun clasker-edit-quit ()
  (interactive)
  (kill-buffer)
  (delete-window)
  (clasker-revert))

(defvar clasker-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'clasker-edit-save-ticket)
    (define-key map (kbd "C-c C-k") 'clasker-edit-quit)
    map)
  "docstring")

(define-derived-mode clasker-edit-mode text-mode "Clasker Edit"
  "docstring")

(provide 'clasker-edit)

;;; clasker-edit.el ends here
