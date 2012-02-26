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
