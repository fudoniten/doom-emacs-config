;;; nix.el -*- lexical-binding: t; -*-
;; Nix language configuration

;;; Code:

;; nix-ts-mode: native tree-sitter mode for Nix
;; Docs: https://github.com/nix-community/nix-ts-mode
(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :config
  (when (locate-library "eglot")
    (after! eglot
      (add-to-list 'eglot-server-programs '(nix-ts-mode . ("nil")))
      (add-hook 'nix-ts-mode-hook #'eglot-ensure))))

;; nixfmt formatter via apheleia (used by format +onsave)
;; Install nixfmt: https://github.com/NixOS/nixfmt
;; Or swap 'nixfmt for 'alejandra if you prefer: https://github.com/kamadorueda/alejandra
(after! apheleia
  (setf (alist-get 'nix-mode    apheleia-mode-alist) 'nixfmt
        (alist-get 'nix-ts-mode apheleia-mode-alist) 'nixfmt)
  (unless (assq 'nixfmt apheleia-formatters)
    (setf (alist-get 'nixfmt apheleia-formatters) '("nixfmt"))))

(provide 'nix-config)
;;; nix.el ends here
