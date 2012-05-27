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
;;;       Specify a hierarchy relationship with other ticket. It is an integer,
;;;       it refers to the ticket stored in the same file with that line number.
;;;
;;;       
;;;
;;; Two generic functions are provided to manipulate the properties of a ticket:
;;; `clasker-ticket-get-property' and `clasker-ticket-set-property'.
;;;
;;; Finally, no every ticket has to be list in the clasker buffer. A buffer
;;; local variable `clasker-view-function' is supposed to keep a function which
;;; will return the list of tickets to show.
;;;

(eval-when-compile
  (unless (<= 24 emacs-major-version)
    (error "You need at least Emacs 24 to use Clasker.")))

(require 'cl)
(require 'eieio)

(defgroup clasker nil
  "Experimental task management."
  :prefix "clasker-"
  :group 'applications)

(defcustom clasker-directory "~/.clasker.d/"
  "Clasker directory"
  :group 'clasker)
(unless(file-exists-p clasker-directory)
  (make-directory clasker-directory))

;;;; Tickets

(defcustom clasker-ticket-file (concat clasker-directory "tickets")
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

(defmethod initialize-instance :after ((ticket clasker-ticket) _slots)
  (unless (eq (class-of ticket) 'clasker-ticket)
    (clasker-ticket-set-class ticket (class-of ticket))))

(defun clasker-subclass-p (class1 class2)
  "Return T if the symbol CLASS1 designates a subclass of the
class whose name is CLASS2. Otherwise return NIL."
  (and (class-p class1)
       (class-p class2)
       (child-of-class-p class1 class2)))

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


;;; This variable keeps a weak hash table which associates ticket identifiers as
;;; (FILENAME lineno) with the ticket objects itself. It is useful to reload
;;; tickets from disks preserving the eq-identity.
(defvar clasker-ticket-table
  (make-hash-table :test 'equal :weakness 'value))

(defun clasker-ticket-id (ticket)
  (list (oref ticket filename) (oref ticket line)))

(defun clasker-intern-ticket (id &optional class)
  (setq class (or class 'clasker-ticket))
  (unless (clasker-subclass-p class 'clasker-ticket)
    (error "unknown-clasker-ticket-class"))
  (or (gethash id clasker-ticket-table)
      (puthash id (make-instance class) clasker-ticket-table)))


(defun clasker--parse-ticket-line (&optional id)
  (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
         (props (ignore-errors (read-from-whole-string (clasker--unquote-string line))))
         (class (or (cdr (assq 'class props)) 'clasker-ticket))
         (ticket (if id (clasker-intern-ticket id class) (make-instance class))))
    (oset ticket filename (car id))
    (oset ticket line (cadr id))
    (dolist (prop props)
      (clasker-ticket-set-property ticket (car prop) (cdr prop)))
    ticket))


;;; Load an individual ticket given by the identifier ID. It could modify
;;; tickets objects, you usually prefer to use `clasker-resolve-id' instead.
(defun clasker-load-id (id)
  (let* ((filename (or (car id) clasker-ticket-file))
        (lineno (cadr id))
        (id `(,(expand-file-name filename) ,lineno)))
    (when (file-readable-p filename)
      (with-temp-buffer
        (insert-file-contents-literally filename)
        (goto-char (point-min))
        (forward-line (1- lineno))
        (clasker--parse-ticket-line id)))))

;;; Resolve a ticket identifier. It is like `clasker-load-id', but it tries not
;;; to load the ticket from disk if it has been loaded already, so it does not
;;; modify ticket objects.
(defun clasker-resolve-id (id)
  (let ((full-id
         (if (integerp id)
             `(,(expand-file-name clasker-ticket-file) ,id)
           id)))
    (or (gethash full-id clasker-ticket-table)
        (clasker-load-id full-id))))


;;;; Properties

(defvar clasker-inhibit-property-hook nil)
(defvar clasker-property-hook-table
  (make-hash-table :test #'eq))

(put 'childs 'clasker-volatile t)

(defun clasker-add-property-hook (property function)
  (pushnew function (gethash property clasker-property-hook-table)))

(defun clasker-run-property-hook (ticket property new-value)
  (unless clasker-inhibit-property-hook
    (dolist (hook (gethash property clasker-property-hook-table))
      (funcall hook ticket property new-value))))

(defmethod clasker-ticket--add-property ((ticket clasker-ticket) property value)
  (object-add-to-list ticket 'properties (cons property value)))

(defmethod clasker-ticket--get-property ((ticket clasker-ticket) property-name)
  (assq property-name (slot-value ticket 'properties)))

(defmethod clasker-ticket--set-property ((ticket clasker-ticket) property value)
  (let ((entry (clasker-ticket--get-property ticket property)))
    (if entry (setcdr entry value)
      (clasker-ticket--add-property ticket property value))))

(defmethod clasker-ticket--delete-property ((ticket clasker-ticket) property)
  (let ((property-list (slot-value ticket 'properties)))
    (oset ticket properties (assq-delete-all property property-list))))

(defun clasker-ticket--get-property-1 (ticket property &optional no-classes)
  (or (clasker-ticket--get-property ticket property)
      (unless no-classes
        (let ((classes (clasker-ticket-classes ticket))
              (value nil))
          (while (and classes (not value))
            (setq value (get (car classes) property))
            (setq classes (rest classes)))
          (cons property value)))))

(defmethod clasker-ticket-get-property ((ticket clasker-ticket) property &optional parents no-classes)
  (setq parents (or parents 0))
  (let (value)
    (block nil
      (while t
        (setq value (clasker-ticket--get-property-1 ticket property no-classes))
        (if value (return)
          (if (eql parents 0) (return)
            (when (numberp parents) (decf parents))
            (setf ticket (clasker-ticket-parent ticket))))))
    (cdr value)))

(defmethod clasker-ticket-set-property ((ticket clasker-ticket) property value)
  (clasker-run-property-hook ticket property value)
  (clasker-ticket--set-property ticket property value))

(defmethod clasker-ticket-delete-property ((ticket clasker-ticket) property)
  (clasker-run-property-hook ticket property nil)
  (clasker-ticket--delete-property ticket property))


(defun clasker-ticket-ancestor-p (ancestor child)
  (while (and child (not (eq ancestor child)))
    (setq child (clasker-ticket-parent child)))
  (eq ancestor child))

(defmethod clasker-save-ticket ((ticket clasker-ticket))
  (let ((line (oref ticket line)))
    (with-temp-file clasker-ticket-file
      (when (file-readable-p clasker-ticket-file)
        (insert-file-contents-literally clasker-ticket-file))
      (if (not line)
          (goto-char (point-max))
        (goto-char (point-min))
        (forward-line (1- line))
        (delete-region (line-beginning-position) (line-end-position)))
      (oset ticket filename (expand-file-name clasker-ticket-file))
      (oset ticket line (line-number-at-pos))
      (let ((properties (oref ticket properties)))
        (insert
         (clasker--quote-string
          (with-output-to-string
            (princ "(")
            (dolist (prop properties)
              (unless (get (car prop) 'clasker-volatile)
                (prin1 prop)))
            (princ ")"))))
        (unless line (insert "\n"))))))


(defmacro clasker-with-collect (&rest body)
  (declare (indent defun) (debug t))
  (let ((head (gensym))
        (tail (gensym)))
    `(let* ((,head (list 'collect))
            (,tail ,head))
       (flet ((collect (x)
                (setf (cdr ,tail) (cons x nil))
                (setf ,tail (cdr ,tail))))
         ,@body)
       (cdr ,head))))


(defun clasker-load-tickets (&optional filename)
  (let ((filename (expand-file-name (or filename clasker-ticket-file))))
    (when (file-readable-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (point-min))
        (clasker-with-collect
          (while (< (point) (point-max))
            (collect (clasker--parse-ticket-line `(,filename ,(line-number-at-pos))))
            (forward-line)))))))


;;; Ticket accessors

(defun clasker-ticket-parent (ticket)
  (let ((refer (clasker-ticket-get-property ticket 'parent nil t)))
    (when refer (clasker-resolve-id refer))))

(defun clasker-update-childs (ticket _property new-value)
  (let ((clasker-inhibit-property-hook t))
    (let ((oldparent (clasker-ticket-parent ticket))
          (newparent (clasker-resolve-id new-value)))
      (when oldparent
        (let ((siblings (remove ticket (clasker-ticket-childs oldparent))))
          (clasker-ticket-set-property oldparent 'childs siblings)))
      (when newparent
        (let ((siblings (cons ticket (clasker-ticket-childs newparent))))
          (clasker-ticket-set-property newparent 'childs siblings))))))
(clasker-add-property-hook 'parent 'clasker-update-childs)


(defun clasker-ticket-childs (ticket)
  (clasker-ticket-get-property ticket 'childs nil t))

(defmethod clasker-ticket-class ((ticket clasker-ticket) value)
  (or (clasker-ticket-get-property ticket 'class value nil t) 'clasker-ticket))

(defmethod clasker-ticket-set-class ((ticket clasker-ticket) value)
  (if (eq value 'clasker-ticket)
      (clasker-ticket-delete-property ticket 'class)
    (clasker-ticket-set-property ticket 'class value)))

(defun clasker-ticket-classes (ticket)
  (clasker-ticket-get-property ticket 'classes nil t))

(defun clasker-ticket-description (ticket)
  (clasker-ticket-get-property ticket 'description))

(defun clasker-ticket-timestamp (ticket)
  (clasker-ticket-get-property ticket 'timestamp))

(defun clasker-ticket-archived-p (ticket)
  (clasker-ticket-get-property ticket 'archived))

(defun clasker-ticket-archived-since (ticket)
  (clasker-ticket-get-property ticket 'archive-timestamp))

(defun clasker-ticket-ago (ticket)
  (let ((timestamp (clasker-ticket-timestamp ticket)))
    (and timestamp (float-time (time-subtract (current-time) timestamp)))))



;;; Operations on text of tickets

(defsubst clasker-ticket-at-point ()
  (get-text-property (point) 'clasker-ticket))

(defun clasker-beginning-of-ticket ()
  (interactive)
  (let ((begin (previous-single-property-change (1+ (point)) 'clasker-ticket)))
    (goto-char (or begin (point-min)))))

(defun clasker-end-of-ticket ()
  (interactive)
  (let ((end  (next-single-property-change (point) 'clasker-ticket)))
    (goto-char (1- (or end (point-max))))))

(defun clasker--following-single-ticket (movement-func)
  (let ((ticket (clasker-ticket-at-point))
        (point (point)))
    (setq next-ticket-pos (funcall movement-func  point 'clasker-ticket))
    (while (and next-ticket-pos
                (or (not (get-text-property next-ticket-pos 'clasker-ticket))
                    (equal next-ticket-pos ticket)))
      (setq next-ticket-pos (funcall movement-func next-ticket-pos 'clasker-ticket)))
    (when next-ticket-pos
      (goto-char next-ticket-pos))))

(defun clasker-next-ticket (&optional arg)
  (interactive "p")
  (setq arg (or arg 1))
  (dotimes (_i arg t)
    (clasker--following-single-ticket 'next-single-property-change)))

(defun clasker-previous-ticket (&optional arg)
  (interactive "p")
  (setq arg (or arg 1))
  (dotimes (_i arg t)
    (clasker--following-single-ticket 'previous-single-property-change)))


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

(defun clasker-action-archive (ticket)
  (clasker-ticket-set-property ticket 'archived t)
  (clasker-ticket-set-property ticket 'archive-timestamp (butlast (current-time)))
  (clasker-save-ticket ticket))

(defun clasker-ticket-actions (ticket)
  clasker-default-actions)

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
        (if (or (null ts1) (null ts2) (equal ts1 ts2))
            (> (oref t2 uid) (oref t1 uid))
          (time-less-p ts2 ts1))))
     ;; Reverse order for archived tickets
     ((and (clasker-ticket-archived-p t1) (clasker-ticket-archived-p t2))
      (if (and (clasker-ticket-get-property t1 'archive-timestamp)
               (clasker-ticket-get-property t2 'archive-timestamp))
          (let ((ts1 (clasker-ticket-get-property t1 'archive-timestamp))
                (ts2 (clasker-ticket-get-property t2 'archive-timestamp)))
            (if (or (null ts1) (null ts2) (equal ts1 ts2))
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
  (sort (clasker-filter-tickets (clasker-load-tickets)) 'clasker-ticket<))

(defun clasker-current-view ()
  (funcall clasker-view-function))


;;; Filters

(defvar clasker-filter-functions nil)

(defun clasker-filtered-ticket-p (ticket)
  (every (lambda (f) (funcall f ticket))
         clasker-filter-functions))

(defun clasker-filter-tickets (ticket-list)
  (remove-if #'clasker-filtered-ticket-p ticket-list))

(defvar clasker-filter-expire 300
  "Number of seconds after an archived ticket become expired.")

(defun clasker-expired-ticket-p (ticket)
  (and (clasker-ticket-archived-p ticket)
       (clasker-ticket-get-property ticket 'archive-timestamp)
       (> (float-time (time-subtract (current-time) (clasker-ticket-archived-since ticket)))
          clasker-filter-expire)))
(pushnew 'clasker-expired-ticket-p clasker-filter-functions)

;;;; User commands and interface

(defvar clasker-title "Clasker"
  "The title of the *clasker* buffer.")

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
              (when (clasker-ticket-at-point)
                (push (clasker-ticket-at-point) tickets))
              (or (clasker-next-ticket) (return))))))
      tickets))
   (t
    (and (clasker-ticket-at-point)
         (list (clasker-ticket-at-point))))))


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
              (princ (format "%2d%s" value1 name1))
              (unless (or (null value2) (zerop value2))
                (princ (format " %02d%s" value2 name2))))))))

(defmethod clasker-ticket-headline ((ticket clasker-ticket))
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
    (insert (propertize clasker-title 'font-lock-face 'info-title-1) "\n\n")
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


(defun clasker-mark-ticket-pos ()
  (interactive)
  (let ((end (clasker-end-of-ticket)))
    (push-mark (clasker-beginning-of-ticket) t nil)
    (goto-char end)))


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
  (let* ((clasker-inhibit-confirm t)
         (tickets (clasker-active-tickets))
         (actions
          (let ((all-actions (mapcar 'clasker-ticket-actions tickets)))
            (flet ((combine (actions1 actions2)
                     (intersection actions1 actions2 :test 'equal)))
              (reduce 'combine all-actions)))))
    (when (and actions tickets)
      (let ((action (clasker-read-action actions)))
        (when action
          (dolist (ticket tickets)
            (funcall action ticket))))))
  (clasker-revert))

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



(defvar current-ticket)

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

(defun clasker-action-edit (ticket)
  (clasker-with-new-window "*Clasker Edit*" 10
    (clasker-edit-mode)
    (set (make-local-variable 'current-ticket) ticket)
    (insert (clasker-ticket-description ticket))))

(defun clasker-edit-save-ticket ()
  (interactive)
  (clasker-ticket-set-property current-ticket 'description (buffer-string))
  (clasker-save-ticket current-ticket)
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


(provide 'clasker)

;;; Local variables:
;;; lexical-binding: t
;;; indent-tabs-mode: nil
;;; fill-column: 80
;;; End:

;;; clasker.el ends here
