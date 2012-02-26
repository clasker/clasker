;;; Clasker.el --- An experimental tracker for Emacs

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

(eval-when-compile
  (require 'cl))

(require 'eieio)

(defgroup clasker nil
  "Experimental task management."
  :prefix "clasker-"
  :group 'applications)

(defcustom clasker-smart-file t
  "when set to t, tries to find a .clasker file in the current directory"
  :type 'boolean
  :group 'clasker)

(defun clasker-find-directory-upwards (from to test &optional if-nil)
  (when (not (file-exists-p from))
    (return))
  (if (or (equal (expand-file-name from) (expand-file-name to))
          (equal from "/")) ;how to do it multiplatform?
      (or if-nil to)
    (if (funcall test from) from
      (clasker-find-directory-upwards (expand-file-name (concat from "/../")) ;how to do it multiplatform?
                                to
                                test
                                if-nil))))

(defcustom clasker-file "~/.clasker"
  "File where clasker file tickets are"
  :type 'file
  :group 'clasker
  :set-after '(clasker-smart-file)
  :initialize (lambda (symbol value)
                (let ((path
                       (if clasker-smart-file
                           (clasker-find-directory-upwards
                            default-directory
                            "~/"
                            (lambda (x) (file-exists-p (concat x ".clasker") )))
                         default-directory)))
                  (setq clasker-file (concat path ".clasker")))))




;;;; Tickets

(defclass clasker-ticket ()
  ((properties :initarg :properties
               :initform ()
               :type list
               :documentation "property alist")))

(defmethod clasker-ticket-get-property ((ticket clasker-ticket) property-name)
  (cdr (assq property-name (slot-value ticket 'properties))))

(defmethod clasker-ticket-set-property ((ticket clasker-ticket) property value)
  (if (assoc property (slot-value ticket 'properties))
      (setcdr (assoc property (slot-value ticket 'properties)) value)
    (clasker-ticket--add-property ticket property value)))

(defmethod clasker-ticket--add-property ((ticket clasker-ticket) property value)
  (object-add-to-list ticket 'properties (cons property value)))


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
  (clasker-ticket-get-property ticket 'description))

(defun clasker-ticket-ago (ticket)
  (let ((timestamp (clasker-ticket-get-property ticket 'timestamp)))
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
              (when (< n (length actions))
                (setq value (cdr (nth n actions)))
                (setq finishp t)))))))
      (kill-buffer buffer)
      (delete-window window)
      value)))


(defvar clasker-default-actions
  '(("Delete" . clasker-action-delete)
    ("Edit" . clasker-action-edit)))

(defun clasker-action-delete (ticket)
  (setq clasker-tickets (delq ticket clasker-tickets))
  (clasker-save-tickets))

(defun clasker-action-edit (ticket)
  (let ((new-text
         (read-string "New description: "
                      (clasker-ticket-description ticket)
                      nil
                      (clasker-ticket-description ticket))))
    (clasker-ticket-set-property ticket  'description new-text))
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
    (list (get-text-property (point) 'clasker-ticket)))))


(defun clasker-format-seconds (seconds)
  "Format a number of seconds in a readable way."
  (unless (plusp seconds)
    (error "%d is not a possitive number." seconds))
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
      (loop for (name1 name2) on '("y" "d" "h" "m" "s")
            for (value1 value2) on (list years days hours minutes seconds)
            while (zerop value1)
            finally
            (progn
              (princ (format "%d%s" value1 name1))
              (unless (or (null value2) (zerop value2))
                (princ (format "%3d%s" value2 name2))))))))

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
  (let (end ticket)
    (while (not end)
      (let ((description (read-string "Description (or RET to finish): " nil nil :no-more-input)))
        (if (eq description :no-more-input)
            (setf end t)
          (setf ticket (make-instance 'clasker-ticket))
          (clasker-ticket-set-property ticket 'description description)
          (clasker-ticket-set-property ticket 'timestamp (butlast (current-time)))
          (push ticket clasker-tickets)
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

(defun clasker-open-file (arg)
  (interactive "fOpen Clasker file: ")
  (setf clasker-file arg)
  (clasker-revert))

(defvar clasker-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'revert-buffer)
    (define-key map (kbd "c") 'clasker-new-tickets)
    (define-key map (kbd "k") 'clasker-delete-ticket)
    (define-key map (kbd "q") 'clasker-quit)
    (define-key map (kbd "n") 'clasker-next-ticket)
    (define-key map (kbd "p") 'clasker-previous-ticket)
    (define-key map (kbd "C-c C-f") 'clasker-open-file)
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

;; 17:31 <rgc> otra pregunta: no veo clara la diferencia entre plists y alists
;; 18:09 <kehoea> rgc, se trata de la estructura
;; 18:10 <kehoea> plist: (nombre1 valor1 nombre2 valor2)
;; 18:10 <kehoea> alist: ((nombre1 . valor1) (nombre2 . valor2))
;; 18:11 <kehoea> (plist-get 'nombre1 plist) => valor1
;; 18:11 <kehoea> (assq 'nombre1 alist) => (nombre1 . valor1)
