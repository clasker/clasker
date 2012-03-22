(defun clasker-filter-github (ticket)
  (clasker-github-ticket-p ticket))

(defun clasker-filter-recent ()
  (let ((timestamp (string-to-int (read-string "number:"))))
    (lambda (ticket) (<= timestamp (car (clasker-ticket-get-property ticket 'timestamp))))))

(defvar clasker-default-filters ())

(setq clasker-default-filters (list #'clasker-filter-github (clasker-filter-recent)))

(defun clasker-ticket-filtered (ticket &optional filters)
  (let ((ti ticket))
    (catch 'break
      (dolist (f (or filters clasker-default-filters) ti)
        (unless (funcall f ti)
          (throw 'break  t))))))

(defun clasker-filter-all-tickets ()
  (let ((tickets))
    (dolist (ticket (clasker-load-tickets))
      (unless (clasker-ticket-filtered ticket)
        (push ticket tickets)))
    (nreverse tickets)))

(with-current-buffer "*Clasker*"
 (setq clasker-view-function 'clasker-filter-all-tickets))

(provide 'clasker-configs)

;;; Local variables:
;;; lexical-binding: t
;;; indent-tabs-mode: nil
;;; fill-column: 80
;;; End:

;;; clasker-configs.el ends here
