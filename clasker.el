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

(require 'eieio)

(defcustom clasker-file "~/.clasker"
  "File where clasker file tickets are")


;;;; Tickets

(defvar clasker-tickets nil
  "tickets")

(defun clasker-show-ticket (ticket)
  (let ((description (cdr (assq 'description ticket)))
        (duration
         (let ((timestamp (cdr (assq 'timestamp ticket))))
           (if timestamp
               (format-seconds "%dd %hh %ss%z" (truncate (float-time (time-subtract (current-time) timestamp))))
             ""))))
    (insert (propertize (format "%s%60s\n" description duration)
                        'clasker-ticket ticket))))

(defun clasker-show-tickets (list)
  (dolist (ticket list)
    (clasker-show-ticket ticket)))

(defun clasker-load-tickets (&optional filename)
  (let ((filename (or filename clasker-file)))
    (when (file-readable-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (read (current-buffer))))))

(defun clasker-save-tickets (&optional filename)
  (with-temp-file (or filename clasker-file)
    (insert ";; This file is generated automatically. Do NOT edit!\n")
    (prin1 clasker-tickets #'insert)))

(defun clasker-revert (&optional ignore-auto noconfirm)
  (widen)
  (let ((position (point))
        (inhibit-read-only t))
    (erase-buffer)
    (setq clasker-tickets (clasker-load-tickets))
    (insert (propertize "Clasker\n" 'face 'info-title-1) "\n")
    (clasker-show-tickets clasker-tickets)
    (goto-char (min position (point-max)))))

(defun clasker-quit ()
  (interactive)
  (kill-buffer "*Clasker*"))

(defun clasker-new-ticket ()
  (interactive)
  (let* ((ticket-desc (read-string "Description: "))
         (ticket
          `((description . , ticket-desc)
            (timestamp . ,(butlast (current-time))))))
    (push ticket clasker-tickets))
  (clasker-save-tickets)
  (clasker-revert))

(defun clasker-delete-ticket ()
  (interactive)
  (let ((ticket (get-text-property (point) 'clasker-ticket)))
    (when (and ticket (yes-or-no-p "Do you want to delete this ticket? "))
      (setq clasker-tickets (delq ticket clasker-tickets))
      (clasker-save-tickets)
      (clasker-revert))))

(defun clasker-next-ticket ()
  (interactive)
  (let ((position (next-single-property-change (point) 'clasker-ticket)))
    (and position (goto-char position))))

(defun clasker-previous-ticket ()
  (interactive)
  (let ((position (previous-single-property-change (point) 'clasker-ticket)))
    (and position (goto-char position))))

(defvar clasker-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'revert-buffer)
    (define-key map (kbd "c") 'clasker-new-ticket)
    (define-key map (kbd "k") 'clasker-delete-ticket)
    (define-key map (kbd "q") 'clasker-quit)
    (define-key map (kbd "n") 'clasker-next-ticket)
    (define-key map (kbd "p") 'clasker-previous-ticket)
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
