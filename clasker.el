;;; clasker.el --- An experimental tracker for Emacs

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

;;; A ticket is an (direct or indirect) instance of the class
;;; `clasker-ticket'. Tickets have an association list of properties
;;; and values, which can be customized by clasker modules or the
;;; user. Some properties have a special meaning in clasker, as they
;;; are used across the whole project.
;;;
;;;    DESCRIPTION
;;;
;;;    TIMESTAMP
;;;
;;; The functions `clasker-ticket-get-property' and
;;; `clasker-ticket-set-property' are provided to manipulate the
;;; properties of a ticket.
;;;
;;; The variable `clasker-file' keeps the name of the file which
;;; stores all the tickets in the system.
;;;
;;; Tickets can be created by the user, but they are more often
;;; collected from several _sources_. Sources could be other local
;;; files, remote task systems, and so on. Tickets remember the source
;;; from which they are fetched to allow optional ticket
;;; synchronization. Sources are described by instances of the
;;; `clasker-source' class. The generic functions
;;; `clasker-source-fetch-tickets' and `clasker-source-push-ticket'
;;; are provided for synchronization.
;;; 
;;; Finally, no every ticket has to be list in the clasker buffer. A
;;; buffer-local variable `clasker-view-function' is supposed to keep
;;; a function which will return the list of tickets to show.
;;; 

(eval-when-compile
  (require 'cl))

(require 'eieio)
(require 'clasker-edit)

(defgroup clasker nil
  "Experimental task management."
  :prefix "clasker-"
  :group 'applications)

(defcustom clasker-file "~/.clasker"
  "File where clasker file tickets are"
  :type 'file
  :group 'clasker)


;;;; Tickets

(defclass clasker-ticket ()
  ((properties
    :initarg :properties
    :initform ()
    :type list
    :documentation "property alist")))


(defgeneric clasker-source-fetch (source))
(defgeneric clasker-source-push (source ticket))


(defmethod clasker-ticket--add-property ((ticket clasker-ticket) property value)
  (object-add-to-list ticket 'properties (cons property value)))

(defmethod clasker-ticket-get-property ((ticket clasker-ticket) property-name)
  (cdr (assq property-name (slot-value ticket 'properties))))

(defmethod clasker-ticket-set-property ((ticket clasker-ticket) property value)
  (if (assoc property (slot-value ticket 'properties))
      (setcdr (assoc property (slot-value ticket 'properties)) value)
    (clasker-ticket--add-property ticket property value)))

;;; Some special properties

(defun clasker-ticket-description (ticket)
  (clasker-ticket-get-property ticket 'description))

(defun clasker-ticket-ago (ticket)
  (let ((timestamp (clasker-ticket-get-property ticket 'timestamp)))
    (and timestamp (float-time (time-subtract (current-time) timestamp)))))


(defclass clasker-ticket-local (clasker-ticket)
  ((filename :type (or string null) :initform nil)
   (line :type (or integer null) :initform nil)))

(defun clasker-load-tickets (&optional filename)
  (let ((filename (or filename clasker-file)))
    (when (file-readable-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (point-min))
        (let ((finishp nil)
              (tickets nil))
          (while (< (point) (point-max))
            (ignore-errors
              (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
                     (props (read-from-whole-string line))
                     (ticket (make-instance 'clasker-ticket-local :properties props)))
                (oset ticket filename filename)
                (oset ticket line (line-number-at-pos))
                (push ticket tickets)))
            (forward-line))
          (nreverse tickets))))))

(defmethod clasker-save-ticket ((ticket clasker-ticket-local))
  (let ((file (or (oref ticket filename) clasker-file))
        (line (oref ticket line)))
    (with-temp-file file
      (insert-file-contents-literally file)
      (when (= (point-min) (point-max))
        (insert ";; This file is generated automatically. Do NOT edit!\n"))
      (if (not line)
          (goto-char (point-min))
        (goto-line line)
        (delete-region (line-beginning-position) (line-end-position)))
      (oset ticket filename file)
      (oset ticket line (line-number-at-pos))
      (let ((standard-output (current-buffer)))
        (prin1 (oref ticket properties) #'insert)
        (newline)))))

(defmethod clasker-delete-ticket ((ticket clasker-ticket-local))
  (let ((file (or (oref ticket filename) clasker-file))
        (line (oref ticket line)))
    (with-temp-file file
      (insert-file-contents-literally file)
      (when line
        (goto-line line)
        (delete-region (line-beginning-position) (line-end-position))
        (newline)))))


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

(defmacro clasker-with-new-window (buffer-name height &rest body)
  (declare (indent 1))
  (let ((window (make-symbol "window"))
        (buffer (make-symbol "buffer"))
        (returnval (make-symbol "returnval")))
    `(save-excursion
       (let ((,window (split-window-vertically (- (window-height) ,height)))
             (,buffer (generate-new-buffer ,buffer-name)))
         (select-window ,window)
         (switch-to-buffer ,buffer t)
         (erase-buffer)
         ,@body))))

(defun clasker-action-edit (ticket)
  (clasker-with-new-window "*Clasker Edit*" 10
    (clasker-edit-mode)
    (set (make-local-variable 'current-ticket) ticket)
    (insert (clasker-ticket-description ticket))))



;;;; Views

(defvar clasker-view-function
  'clasker-default-view
  "The value of this variable is a function which returns the
list of tickets to be shown in the current view.")

(defun clasker-default-view ()
  (clasker-load-tickets))

(defun clasker-current-view ()
  (funcall clasker-view-function))


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

(defun clasker-ticket-headline (ticket)
  (let* ((lines (split-string (clasker-ticket-description ticket) "\n"))
         (header (first lines)))
    (if (< 1 (length lines))
        (concat header " ...")
      header)))

(defun clasker-show-ticket (ticket)
  (let ((description (clasker-ticket-headline ticket) )
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

(defun clasker-revert (&optional ignore-auto noconfirm)
  (widen)
  (let ((position (point))
        (inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Clasker\n" 'face 'info-title-1) "\n")
    (clasker-show-tickets (clasker-current-view ))
    (goto-char (min position (point-max)))))

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
          (setf ticket (make-instance 'clasker-ticket-local))
          (clasker-ticket-set-property ticket 'description description)
          (clasker-ticket-set-property ticket 'timestamp (butlast (current-time)))
          (clasker-save-ticket ticket)
          (clasker-revert))))))


(defun clasker-delete-ticket* ()
  (interactive)
  (let ((ticket (get-text-property (point) 'clasker-ticket)))
    (when (and ticket (clasker-confirm "Do you want to delete this ticket? "))
      (clasker-action-delete ticket)
      (clasker-revert))))

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
  (clasker-revert))

(defun clasker-open-file (arg)
  (interactive "fOpen Clasker file: ")
  (setf clasker-file arg)
  (clasker-revert))

(defvar clasker-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'revert-buffer)
    (define-key map (kbd "c") 'clasker-new-tickets)
    (define-key map (kbd "k") 'clasker-delete-ticket*)
    (define-key map (kbd "q") 'clasker-quit)
    (define-key map (kbd "n") 'clasker-next-ticket)
    (define-key map (kbd "p") 'clasker-previous-ticket)
    (define-key map (kbd "C-c C-f") 'clasker-open-file)
    (define-key map (kbd "RET") 'clasker-do)
    map)
  "docstring")

(define-derived-mode clasker-mode special-mode "Clasker"
  "docstring"
  (set (make-local-variable 'revert-buffer-function) 'clasker-revert)
  (set (make-local-variable 'clasker-view-function) 'clasker-default-view))

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
  
