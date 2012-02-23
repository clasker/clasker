(define-derived-mode clasker-edit-mode text-mode "Clasker Edit"
  "docstring")


(defvar clasker-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'clasker-edit-save-ticket)
    (define-key map (kbd "C-c C-k") 'clasker-edit-quit)
    map)
  "docstring")

(defun clasker-edit-save-ticket ()
  (interactive)
  (setf (cdr (assoc 'description current-ticket))
        (buffer-string))
  (clasker-save-tickets)
  (clasker-edit-quit))

(defun clasker-edit-quit ()
  (interactive)
  (kill-buffer)
  (delete-window)
  (clasker-revert))

(provide 'clasker-edit)
