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

(defgroup clasker nil
  "Experimental task management."
  :prefix "clasker-"
  :group 'applications)

(defcustom clasker-file "~/.clasker"
  "File where clasker file tickets are"
  :type 'file
  :group 'clasker)


;;;; Tickets

(defvar clasker-tickets nil
  "tickets")

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


(defun clasker-ticket-description (ticket)
  (cdr (assq 'description ticket)))

(defun clasker-ticket-ago (ticket)
  (let ((timestamp (cdr (assq 'timestamp ticket))))
    (and timestamp (float-time (time-subtract (current-time) timestamp)))))


;;;; Actions

(defvar clasker-inhibit-confirm nil)

(defun clasker-confirm (prompt)
  (or clasker-inhibit-confirm (yes-or-no-p prompt)))

(defun clasker-read-action (actions)
  "Read an action from keyboard."
  (save-excursion
    (let ((window (split-window-vertically (- (window-height) 4)))
          (value nil)
          (finishp nil)
          (buffer (generate-new-buffer "*Clasker actions*")))
      (with-selected-window window
        (switch-to-buffer buffer t)
        (erase-buffer)
        (insert "\nList of actions:")
        (let ((count 0))
          (dolist (action actions)
            (insert (format "  [%d] " count))
            (insert (propertize (car action) 'face 'italic))
            (setq count (1+ count)))))
      (while (not finishp)
        (let ((c (let ((inhibit-quit t))
                   (read-char-exclusive "Action: "))))
          (cond
           ((or (= c ?\C-g) (= c ?q))
            (kill-buffer buffer)
            (delete-window window)
            (setq quit-flag t))
           ((and (<= ?0 c) (<= c ?9))
            (let ((n (string-to-number (string c))))
              (when (<= n (length actions))
                (setq value (cdr (nth n actions)))
                (setq finishp t)))))))
      (kill-buffer buffer)
      (delete-window window)
      value)))


(defvar clasker-default-actions
  '(("Delete" . clasker-action-delete)))

(defun clasker-action-delete (ticket)
  (setq clasker-tickets (delq ticket clasker-tickets))
  (clasker-save-tickets))


;;;; User commands and interface

(defun clasker-active-tickets ()
  "Return a list of active tickets."
  (cond
   ((region-active-p)
    (let (tickets)
      (save-excursion
        (let ((beginning (region-beginning))
              (end (region-end)))
          (goto-char beginning)
          (block nil
            (while (< (point) end)
              (push (get-text-property (point) 'clasker-ticket) tickets)
              (or (clasker-next-ticket) (return))))))
      tickets))
   (t
    (get-text-property (point) 'clasker-ticket))))


(defun clasker-format-seconds (seconds)
  "Format a number of seconds in a readable way."
  (unless (plusp seconds)
    (error "%a is not a possitive number." seconds))
  (let (years days hours minutes)
    (macrolet ((comp (var n)
                     `(progn
                        (setq ,var (truncate (/ seconds ,n)))
                        (setq seconds (mod seconds ,n)))))
      (comp years 31536000)
      (comp days 86400)
      (comp hours 3600)
      (comp minutes 60))
    (with-output-to-string
      (loop with count = 0
            for name across "ydhms"
            for value in (list years days hours minutes seconds)
            while (< count 2)
            when (/= 0 value) do
            (progn
              (when (<= 1 count) (princ " "))
              (princ (format "%2d%c" value name))
              (incf count))))))


(defun clasker-show-ticket (ticket)
  (let ((description (clasker-ticket-description ticket))
        (timestring
         (let ((secs (clasker-ticket-ago ticket)))
           (if secs (clasker-format-seconds secs) ""))))
    (insert (propertize (concat "  "
                                description
                                (make-string (- (window-width) (length description) (length timestring) 3) ?\s)
                                timestring
                                "\n")
                        'clasker-ticket ticket))))

(defun clasker-show-tickets (list)
  (dolist (ticket (reverse list))
    (clasker-show-ticket ticket)))

(defun clasker-render ()
  (widen)
  (let ((position (point))
        (inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Clasker\n" 'face 'info-title-1) "\n")
    (clasker-show-tickets clasker-tickets)
    (goto-char (min position (point-max)))))

(defun clasker-revert (&optional ignore-auto noconfirm)
  (setq clasker-tickets (clasker-load-tickets))
  (clasker-render))

(defun clasker-quit ()
  (interactive)
  (kill-buffer "*Clasker*"))

(defun clasker-new-tickets ()
  (interactive)
  (let (end)
    (while (not end)
      (let ((description (read-string "Description (or RET to finish): " nil nil :no-more-input)))
        (if (eq description :no-more-input)
            (setf end t)
          (push `((description . ,description)
                  (timestamp . ,(butlast (current-time))))
                clasker-tickets)
          (clasker-render))))
    (clasker-save-tickets)))


(defun clasker-delete-ticket ()
  (interactive)
  (let ((ticket (get-text-property (point) 'clasker-ticket)))
    (when (and ticket (clasker-confirm "Do you want to delete this ticket? "))
      (clasker-action-delete ticket)
      (clasker-render))))

(defun clasker-next-ticket ()
  (interactive)
  (let ((position (next-single-property-change (point) 'clasker-ticket)))
    (and position (goto-char position))))

(defun clasker-previous-ticket ()
  (interactive)
  (let ((position (previous-single-property-change (point) 'clasker-ticket)))
    (and position (goto-char position))))

(defun clasker-do ()
  (interactive)
  (let ((clasker-inhibit-confirm t)
        (action (clasker-read-action clasker-default-actions)))
    (when action
      (dolist (ticket (clasker-active-tickets))
        (funcall action ticket))))
  (clasker-render))

(defvar clasker-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'revert-buffer)
    (define-key map (kbd "c") 'clasker-new-tickets)
    (define-key map (kbd "k") 'clasker-delete-ticket)
    (define-key map (kbd "q") 'clasker-quit)
    (define-key map (kbd "n") 'clasker-next-ticket)
    (define-key map (kbd "p") 'clasker-previous-ticket)
    (define-key map (kbd "RET") 'clasker-do)
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
