;;; clasker.el --- An experimental tracker for Emacs

;; Copyright (C) 2012  David Vázquez

;; Author: David Vázquez <davazp@gmail.com>
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

(eval-when-compile
  (require 'cl))

(defcustom *clasker-file* "~/.clasker" "File where clasker file tasks are")

(defvar *clasker-tasks* '() "tasks")

(defun clasker-show-tasks (list)
  (let ((n 0))
    (dolist (title list)
      (insert title "\n")
      (incf n))))

(defun clasker-revert (&optional ignore-auto noconfirm)
  ""
  (widen)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Clasker\n\n" 'face 'bold))
    (clasker-show-tasks (clasker-get-tasks))))

(defun clasker-get-tasks ()
  (interactive)
  (when (null *clasker-tasks*)
    (setf *clasker-tasks* (read-process-file *clasker-file*)))
  *clasker-tasks*)

(defun read-process-file (file)
  (when (file-readable-p file)
    (with-temp-message "Loading tasks..."
      (with-temp-buffer
        (insert-file-contents file)
        (read (current-buffer))))))

(defun clasker-quit ()
  (interactive)
  (kill-buffer "*Clasker*"))

(defun clasker-new-task ()
  (interactive)
  (let ((task-desc (read-string "Description:")))
    (push task-desc *clasker-tasks*))
  (clasker-revert))

(defvar clasker-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'revert-buffer)
    (define-key map (kbd "c") 'clasker-new-task)
    (define-key map (kbd "q") 'clasker-quit)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    map)
  "docstring")



(define-derived-mode clasker-mode special-mode "Clasker"
  "docstring"
  (set (make-local-variable 'revert-buffer-function) 'clasker-revert))

(defun clasker ()
  "docstring"
  (interactive)
  (switch-to-buffer "*Clasker*")
  (clasker-mode)
  (clasker-revert))

(provide 'clasker)
;;; clasker.el ends here
