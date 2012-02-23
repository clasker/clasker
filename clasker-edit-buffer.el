(define-derived-mode clasker-edit-mode text-mode "Clasker Edit"
  "docstring")


(defvar clasker-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'clasker-edit-save-ticket)
    (define-key map (kbd "C-c C-k") 'clasker-edit-quit)
    map)
  "docstring")
