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

;;; A ticket is an (direct or indirect) instance of the class `clasker-ticket'.
;;; Tickets have an association list of properties and values, which can be
;;; customized by clasker modules or the user. Some properties have a special
;;; meaning in clasker, as they are used across the whole project.
;;;
;;;    CLASSES
;;;
;;;        A list of Lisp symbols. When a property is searched in a ticket and
;;;        it is not present, then the propery list of the symbols will be
;;;        browsed in order to find a value for the requested property. So, the
;;;        properties of a symbol are effective properties of the tickets which
;;;        belong to that class.
;;;
;;;    DESCRIPTION
;;;
;;;    TIMESTAMP
;;;
;;;    ARCHIVED
;;;
;;;    ARCHIVE-TIMESTAMP
;;;
;;;    PARENT
;;;
;;;       Specify a hierarchy relationship with other ticket. If it is an
;;;       integer, it refers to the ticket stored in the same file with that
;;;       line number. It could also specify the file explicitly if the value is
;;;       a list as ("<FILENAME>" <integer>). The recommended way to look for a
;;;       property value in nearest ancestor is
;;;       `clasker-ticket-get-property-in-hierarchy'.
;;;
;;;
;;; Two generic functions are provided to manipulate the properties of a ticket:
;;; `clasker-ticket-get-property' and `clasker-ticket-set-property'.
;;;
;;; There's also support for hierarchies in tickets. The recommended way to look
;;;
;;; Tickets can be created by the user, but they are more often collected from
;;; several _sources_. Sources could be other local files, remote task systems,
;;; and so on. Tickets remember the source from which they are fetched to allow
;;; optional ticket synchronization. Sources are described by instances of the
;;; `clasker-source' class. The generic functions `clasker-source-fetch-tickets'
;;; and `clasker-source-push-ticket' are provided for synchronization.
;;;
;;; Finally, no every ticket has to be list in the clasker buffer. A buffer
;;; local variable `clasker-view-function' is supposed to keep a function which
;;; will return the list of tickets to show.
;;;
;;; TODO: Remove after testing filters
;;; (defun clasker-filter-gh ()
;;;  (sort (remove-if-not (lambda (x) (clasker-ticket-get-property x 'github-id))
;;;                       (clasker-load-tickets))
;;;        'clasker-ticket<))


(eval-when-compile
  (require 'cl))

(eval-when-compile
  (unless (<= 24 emacs-major-version)
    (error "You need at least Emacs 24 to use Clasker.")))

(require 'eieio)
(require 'clasker-edit)

(defgroup clasker nil
  "Experimental task management."
  :prefix "clasker-"
  :group 'applications)



;;;; Tickets

(defcustom clasker-file "~/.clasker"
  "File where clasker file tickets are"
  :type 'file
  :group 'clasker)

(defvar clasker-ticket-counter 0)
(defclass clasker-ticket ()
  (;; File and line where the ticket lives. They are used to save the changes to
   ;; a ticket. If both are NIL, then the ticket is volatile and will not be
   ;; saved across sessions.
   (filename
    :initarg :filename
    :initform nil
    :type (or string null))
   (line
    :initarg :line
    :initform nil
    :type (or integer null))
   ;; UID is an unique identifier for the ticket for the current emacs session.
   (uid
    :initform (incf clasker-ticket-counter))
   ;; Association list for this ticket.
   (properties
    :initarg :properties
    :initform ()
    :type list)))

;;; This variable keeps a weak hash table which associates ticket identifiers as
;;; (FILENAME lineno) with the ticket objects itself. It is useful to reload
;;; tickets from disks preserving the eq-identity.
(defvar clasker-ticket-table
  (make-hash-table :test 'equal :weakness 'value))

(defun clasker-ticket-id (ticket)
  (list (oref ticket filename) (oref ticket line)))

(defun clasker-intern-ticket (id)
  (or (gethash id clasker-ticket-table)
      (puthash id (make-instance 'clasker-ticket) clasker-ticket-table)))

;;; Load an individual ticket given by the identifier ID. It could modify
;;; tickets objects, you usually prefer to use `clasker-resolve-id' instead.
(defun clasker-load-id (id)
  (let* ((filename (or (car id) clasker-file))
        (lineno (cadr id))
        (id `(,(expand-file-name filename) ,lineno)))
    (when (file-readable-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (point-min))
        (forward-line (1- lineno))
        (ignore-errors
          (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
                 (raw-props (read-from-whole-string line))
                 (props (mapcar (lambda (prop)
                                  (cons (car prop)
                                        (if (stringp (cdr prop))
                                            (clasker--unquote-string (cdr prop))
                                          (cdr prop))))
                                raw-props))
                 (ticket (clasker-intern-ticket id)))
            (oset ticket properties props)
            (oset ticket filename filename)
            (oset ticket line (line-number-at-pos))
            ticket))))))

;;; Resolve a ticket identifier. It is like `clasker-load-id', but it tries not
;;; to load the ticket from disk if it has been loaded already, so it does not
;;; modify ticket objects.
(defun clasker-resolve-id (id)
  (let ((full-id
         (if (integerp id)
             `(,(expand-file-name clasker-file) ,id)
           id)))
    (or (gethash full-id clasker-ticket-table)
        (clasker-load-id full-id))))

(defmethod clasker-ticket--add-property ((ticket clasker-ticket) property value)
  (object-add-to-list ticket 'properties (cons property value)))

(defmethod clasker-ticket-get-property ((ticket clasker-ticket) property-name &optional no-recursive)
  (let ((direct-entry (assq property-name (slot-value ticket 'properties))))
    (if direct-entry
        (cdr direct-entry)
      (unless no-recursive
        (let ((classes (clasker-ticket-classes ticket))
              (prop nil))
          (while (and classes (not prop))
            (setq prop (get (first classes) property-name))
            (setq classes (rest classes)))
          prop)))))

(defmethod clasker-ticket-set-property ((ticket clasker-ticket) property value)
  (if (assoc property (slot-value ticket 'properties))
      (setcdr (assoc property (slot-value ticket 'properties)) value)
    (clasker-ticket--add-property ticket property value)))

(defmethod clasker-ticket-delete-property ((ticket clasker-ticket) property)
  (let ((property-list (slot-value ticket 'properties)))
    (when (assoc property property-list)
      (oset ticket properties (assq-delete-all property (slot-value ticket 'properties))))))

(defmethod clasker-ticket-get-property-in-hierarchy ((ticket clasker-ticket) property)
   (let ((ticket-property (clasker-ticket-get-property ticket property)))
     (or ticket-property
         (when (clasker-ticket-parent ticket)
           (clasker-ticket-get-property-in-hierarchy
            (clasker-ticket-parent ticket)
            property)))))

(defun clasker-ticket-ancestor-p (ancestor child)
  (let ((ancestor-id (oref ancestor line)))
    (or
     (equal ancestor-id (oref child line))
     (if (clasker-ticket-parent child)
         (clasker-ticket-ancestor-p ancestor (clasker-ticket-parent child))
       nil))))

(defun clasker--quote-string (string)
  (replace-regexp-in-string "\n" "\\\\n"
   (replace-regexp-in-string "\\\\" "\\\\\\\\" string)))

(defun clasker--unquote-string (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (search-forward "\\" nil t)
      (case (char-after)
        (?n
         (delete-char -1)
         (delete-char 1)
         (newline))
        (t
         (delete-char -1)
         (forward-char))))
    (buffer-string)))

(defmethod clasker-save-ticket ((ticket clasker-ticket))
  (let ((line (oref ticket line)))
    (with-temp-file clasker-file
      (insert-file-contents-literally clasker-file)
      (if (not line)
          (goto-char (point-max))
        (goto-char (point-min))
        (forward-line (1- line))
        (delete-region (line-beginning-position) (line-end-position)))
      (oset ticket filename clasker-file)
      (oset ticket line (line-number-at-pos))
      (let ((standard-output (current-buffer)))
        (insert "(")
        (dolist (property (oref ticket properties))
          (let ((key (symbol-name (car property)))
                (value (typecase (cdr property)
                         (string (prin1-to-string (clasker--quote-string (cdr property))))
                         (t (prin1-to-string (cdr property))))))
            (insert "(" key " . " value ")")))
        (insert ")")
        (unless line (insert "\n"))))))


(defun clasker-load-tickets (&optional filename)
  (let ((filename (or filename clasker-file)))
    (when (file-readable-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (point-min))
        (let ((tickets nil))
          (while (< (point) (point-max))
            (ignore-errors
              (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
                     (raw-props (read-from-whole-string line))
                     (props (mapcar (lambda (prop)
                                      (cons (car prop)
                                            (if (stringp (cdr prop))
                                                (clasker--unquote-string (cdr prop))
                                              (cdr prop))))
                                    raw-props))
                     (ticket (clasker-intern-ticket (list filename (line-number-at-pos)))))
                (oset ticket properties props)
                (oset ticket filename filename)
                (oset ticket line (line-number-at-pos))
                (push ticket tickets)))
            (forward-line))
          (nreverse tickets))))))


;;; Ticket accessors

(defun clasker-ticket-classes (ticket)
  (clasker-ticket-get-property ticket 'classes t))

(defun clasker-ticket-description (ticket)
  (clasker-ticket-get-property ticket 'description))

(defun clasker-ticket-parent (ticket)
  (let ((refer (clasker-ticket-get-property ticket 'parent t)))
    (when refer (clasker-resolve-id refer))))

(defun clasker-ticket-timestamp (ticket)
  (clasker-ticket-get-property ticket 'timestamp))

(defun clasker-ticket-archived-p (ticket)
  (clasker-ticket-get-property ticket 'archived))

(defun clasker-ticket-ago (ticket)
  (let ((timestamp (clasker-ticket-timestamp ticket)))
    (and timestamp (float-time (time-subtract (current-time) timestamp)))))


;;;; Sources and Backends

(defclass clasker-source ()
  nil)

(defgeneric clasker-source-fetch-tickets (source))
(defgeneric clasker-source-push-ticket (source ticket))

;;;; Actions

(defvar clasker-default-actions
  '(("Archive" . clasker-action-archive)
    ("Edit" . clasker-action-edit)))

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


(defmacro clasker-with-new-window (buffer-name height &rest body)
  (declare (indent 1))
  (let ((window (make-symbol "window"))
        (buffer (make-symbol "buffer")))
    `(save-excursion
       (let ((,window (split-window-vertically (- (window-height) ,height)))
             (,buffer (generate-new-buffer ,buffer-name)))
         (select-window ,window)
         (switch-to-buffer ,buffer t)
         (erase-buffer)
         ,@body))))

(defvar current-ticket)
(defun clasker-action-edit (ticket)
  (clasker-with-new-window "*Clasker Edit*" 10
    (clasker-edit-mode)
    (set (make-local-variable 'current-ticket) ticket)
    (insert (clasker-ticket-description ticket))))

(defun clasker-action-archive (ticket)
  (clasker-ticket-set-property ticket 'archived t)
  (clasker-ticket-set-property ticket 'archive-timestamp (butlast (current-time)))
  (clasker-save-ticket ticket))

;;;; Views

(defvar clasker-view-function
  'clasker-default-view
  "The value of this variable is a function which returns the
list of tickets to be shown in the current view.")

(defun clasker-ticket< (t1 t2)
  (cond
   ;; Siblings order
   ((eq (clasker-ticket-parent t1) (clasker-ticket-parent t2))
    (cond
     ;; Oldest to newest
     ((and (not (clasker-ticket-archived-p t1)) (not (clasker-ticket-archived-p t2)))
      (let ((ts1 (clasker-ticket-get-property t2 'timestamp))
            (ts2 (clasker-ticket-get-property t1 'timestamp)))
        (if (equal ts1 ts2)
            (> (oref t2 uid) (oref t1 uid))
          (time-less-p ts2 ts1))))
     ;; Reverse order for archived tickets
     ((and (clasker-ticket-archived-p t1) (clasker-ticket-archived-p t2))
      (if (and (clasker-ticket-get-property t1 'archive-timestamp)
               (clasker-ticket-get-property t2 'archive-timestamp))
          (let ((ts1 (clasker-ticket-get-property t1 'archive-timestamp))
                (ts2 (clasker-ticket-get-property t2 'archive-timestamp)))
            (if (equal ts1 ts2)
                (> (oref t2 uid) (oref t1 uid))
              (time-less-p ts2 ts1)))))
     (t
      (clasker-ticket-archived-p t2))))
   ;; Parents before childs
   ((eq (clasker-ticket-parent t1) t2) nil)
   ((eq (clasker-ticket-parent t2) t1) t)
   (t ;; Preserve tree structure
    (let ((l1 (clasker-ticket-level t1))
          (l2 (clasker-ticket-level t2)))
      (when (>= l1 l2)
        (setq t1 (clasker-ticket-parent t1)))
      (when (>= l2 l1)
        (setq t2 (clasker-ticket-parent t2)))
      (clasker-ticket< t1 t2)))))

(defun clasker-default-view ()
  (sort (clasker-filter-tickets (clasker-load-tickets))
        'clasker-ticket<))

(defun clasker-current-view ()
  (funcall clasker-view-function))


;;; Filtering

(defvar clasker-default-filters nil
  "list of default filters to apply")
(defvar clasker-active-filters clasker-default-filters
  "list of current active filters to apply")

(defun clasker-filter-add-filter (callable-filter &optional filter-list)
  (interactive "afilter:")
  (let ((l (or filter-list 'clasker-active-filters)))
    (add-to-list l callable-filter)
    (clasker-revert)))

(defun clasker-filter-add-filter-lisp (callable-filter &optional filter-list)
  (interactive "Xlisp code to add:")
  (let ((l (or filter-list 'clasker-active-filters)))
    (add-to-list l callable-filter)
    (clasker-revert)))

(defun clasker-ticket-filtered (ticket filters)
  (let ((break (gensym)))
    (catch break
      (dolist (f filters nil)
        (unless (funcall f ticket)
          (throw break t))))))

(defun clasker-filter-tickets (ticket-list)
  (let* ((list (list 'list-of-tickets))
         (tail (last list)))
    (dolist (ticket ticket-list)
      (unless (clasker-ticket-filtered ticket clasker-active-filters)
        (setf (cdr tail) (list ticket))
        (setf tail (cdr tail))))
    (rest list)))

(defun clasker-filter-only ()
  (interactive)
  (let ((ticket (clasker-ticket-at-point)))
    (clasker-filter-add-filter (lambda (x) (clasker-ticket-ancestor-p ticket x)))))

(defun clasker-remove-filters ()
  (interactive)
  (setq clasker-active-filters clasker-default-filters)
  (clasker-revert))


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
              (push (clasker-ticket-at-point) tickets)
              (or (clasker-next-ticket) (return))))))
      tickets))
   (t
    (list (clasker-ticket-at-point)))))


(defun clasker-format-seconds (seconds)
  "Format a number of seconds in a readable way."
  (unless (plusp seconds)
    (error "%d is not a positive number." seconds))
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
                (princ (format " %d%s" value2 name2))))))))

(defun clasker-ticket-headline (ticket)
  (let* ((lines (split-string (clasker-ticket-description ticket) "\n"))
         (header (first lines)))
    (if (< 1 (length lines))
        (concat header " ...")
      header)))

(defun clasker-ticket-level (ticket)
  (let ((level 0))
    (setq ticket (clasker-ticket-parent ticket))
    (while ticket
      (setq ticket (clasker-ticket-parent ticket))
      (setq level (1+ level)))
    level))

(defun clasker-show-ticket (ticket)
  (let ((description (clasker-ticket-headline ticket))
        (timestring
         (let ((secs (clasker-ticket-ago ticket)))
           (if secs (clasker-format-seconds secs) ""))))
    (insert (propertize (concat (make-string (* 3 (1+ (clasker-ticket-level ticket))) ?\s)
                                description
                                (make-string (max 0 (- (window-width)
                                                       (* 3 (1+ (clasker-ticket-level ticket)))
                                                       (length description)
                                                       (length timestring) 3)) ?\s)
                                (propertize timestring 'font-lock-face 'compilation-info)
                                "\n")
                        'clasker-ticket ticket
                        'font-lock-face (if (clasker-ticket-archived-p ticket) 'shadow nil)))))

(defun clasker-show-tickets (list)
  (dolist (ticket list)
    (clasker-show-ticket ticket)))

(defun clasker-revert (&optional _ignore-auto _noconfirm)
  (widen)
  (let ((position (point))
        (inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Clasker\n" 'font-lock-face 'info-title-1) "\n")
    (clasker-show-tickets (clasker-current-view))
    (run-hooks 'clasker-display-hook)
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
          (setf ticket (make-instance 'clasker-ticket))
          (clasker-ticket-set-property ticket 'description description)
          (clasker-ticket-set-property ticket 'timestamp (butlast (current-time)))
          (clasker-save-ticket ticket)
          (clasker-revert))))))

(defun clasker-new-child-tickets ()
  (interactive)
  (let ((actives (clasker-active-tickets))
        parent-id end ticket)
    (if (= (length actives) 1)
        (setq parent-id (clasker-ticket-id (first actives)))
      (error "The parent for the tickets is not well defined."))
    (while (not end)
      (let ((description (read-string "Description (or RET to finish): " nil nil :no-more-input)))
        (if (eq description :no-more-input)
            (setf end t)
          (setf ticket (make-instance 'clasker-ticket))
          (clasker-ticket-set-property ticket 'description description)
          (clasker-ticket-set-property ticket 'timestamp (butlast (current-time)))
          (clasker-ticket-set-property ticket 'parent parent-id)
          (clasker-save-ticket ticket)
          (clasker-revert))))))


(defun clasker-beginning-of-ticket ()
  (interactive)
  (goto-char (or (previous-single-property-change (1+ (point)) 'clasker-ticket)
       (point-min))))

(defun clasker-end-of-ticket ()
  (interactive)
  (let ((end  (next-single-property-change (point) 'clasker-ticket)))
    (goto-char  (1- (or end
                     (point-max))))))

(defun clasker-mark-ticket-pos ()
  (interactive)
  (let ((end (clasker-end-of-ticket)))
    (push-mark (clasker-beginning-of-ticket) t nil)
    (goto-char end)))

(defun clasker-next-ticket (&optional arg)
    (interactive "p")
  (setq arg (or arg 1))
  (dotimes (i arg) (clasker--following-single-ticket
                    'next-single-property-change)))

(defun clasker-previous-ticket (&optional arg)
    (interactive "p")
  (setq arg (or arg 1))
  (dotimes (i arg) (clasker--following-single-ticket
                    'previous-single-property-change)))

(defun clasker--following-single-ticket (movement-func)
  (let ((ticket (clasker-ticket-at-point))
        (point (point)))
    (setq next-ticket-pos (funcall movement-func  point 'clasker-ticket))
    (while (and next-ticket-pos
                (or (not (get-text-property next-ticket-pos 'clasker-ticket))
                    (equal next-ticket-pos ticket)))
      (setq next-ticket-pos (funcall movement-func point 'clasker-ticket)))
    (goto-char (if next-ticket-pos next-ticket-pos point))))

(defun clasker-mark-ticket (arg)
  ;; TODO: Fix to work with negative arguments.
  (interactive "p")
  (when (clasker-ticket-at-point)
    (beginning-of-line)
    (let ((inhibit-read-only t))
      (delete-char 1)
      (insert "*")
      (setq arg (1- arg)))
    (clasker-next-ticket)))

(defun clasker-unmark-ticket (arg)
  ;; TODO: Fix to work with negative arguments.
  (interactive "p")
  (dotimes (_i arg)
    (when (clasker-ticket-at-point)
      (beginning-of-line)
      (let ((inhibit-read-only t))
        (delete-char 1)
        (insert " ")
        (setq arg (1- arg)))
      (clasker-next-ticket))))

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

(defun clasker-ticket-at-point ()
  (get-text-property (point) 'clasker-ticket))

(defvar clasker-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'revert-buffer)
    (define-key map (kbd "c") 'clasker-new-tickets)
    (define-key map (kbd "C") 'clasker-new-child-tickets)
    (define-key map (kbd "q") 'clasker-quit)
    (define-key map (kbd "n") 'clasker-next-ticket)
    (define-key map (kbd "m") 'clasker-mark-ticket)
    (define-key map (kbd "u") 'clasker-unmark-ticket)
    (define-key map (kbd "p") 'clasker-previous-ticket)
    (define-key map (kbd "C-c C-f") 'clasker-open-file)
    (define-key map (kbd "o") 'clasker-filter-only)
    (define-key map (kbd "^") 'clasker-remove-filters)
    (define-key map (kbd "RET") 'clasker-do)
    map)
  "docstring")

(defvar clasker-font-lock-keywords
  (list
   (list "^\\(\\*\\).*$" '(1 dired-mark-face)))
  "Additional expressions to highlight in Clasker mode.")

(define-derived-mode clasker-mode special-mode "Clasker"
  "docstring"
  (set (make-local-variable 'revert-buffer-function) 'clasker-revert)
  (set (make-local-variable 'font-lock-defaults) '(clasker-font-lock-keywords t nil nil beginning-of-line))
  (set (make-local-variable 'clasker-view-function) 'clasker-default-view))

(defun clasker ()
  "docstring"
  (interactive)
  (switch-to-buffer "*Clasker*")
  (clasker-mode)
  (clasker-revert))

(provide 'clasker)

;;; Local variables:
;;; lexical-binding: t
;;; indent-tabs-mode: nil
;;; fill-column: 80
;;; End:

;;; clasker.el ends here
