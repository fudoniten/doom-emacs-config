;;; org.el -*- lexical-binding: t; -*-
;; Org-mode configuration

;;; Code:

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

;; org-modern is enabled by the `+pretty' flag on :lang org in init.el.

(provide 'org-config)
;;; org.el ends here
