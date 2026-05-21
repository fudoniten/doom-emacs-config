;;; org.el -*- lexical-binding: t; -*-
;; Org-mode configuration

;;; Code:

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

;; org-modern: beautiful modern Org appearance
;; Docs: https://github.com/minad/org-modern
(after! org
  (use-package org-modern
    :config (global-org-modern-mode)))

(provide 'org-config)
;;; org.el ends here
