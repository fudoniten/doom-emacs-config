;;; python.el -*- lexical-binding: t; -*-
;; Python configuration
;;
;; pet (Python Environment Tool): https://github.com/wyuenho/emacs-pet
;;   Automatically detects and activates the right interpreter/virtualenv
;;   from pyproject.toml, poetry.lock, Pipfile, requirements.txt,
;;   .python-version, etc. No manual venv activation needed.
;;
;; ruff: https://docs.astral.sh/ruff/
;;   Extremely fast Python linter and formatter (replaces black + isort + flake8).
;;   Install via: pip install ruff  or  nix-env -iA nixpkgs.ruff

;;; Code:

;; pet: automatic virtualenv/interpreter detection per project
(use-package pet
  :config
  (add-hook 'python-base-mode-hook #'pet-mode -10))

;; ruff formatter via apheleia (triggered by format +onsave module)
(after! apheleia
  (setf (alist-get 'python-mode    apheleia-mode-alist) 'ruff
        (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff))

(provide 'python-config)
;;; python.el ends here
