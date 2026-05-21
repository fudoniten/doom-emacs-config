;;; tools.el -*- lexical-binding: t; -*-
;; Miscellaneous tool configuration

;;; Code:

;; diff-hl: highlight uncommitted changes (+/-/~) in the fringe
;; Docs: https://github.com/dgutov/diff-hl
;; Replaces vc-gutter which had compatibility issues
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (after! magit
    (add-hook 'magit-pre-refresh-hook  #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;; magit-delta: syntax-highlighted diffs in Magit using delta
;; Requires delta: https://github.com/dandavison/delta
;; Install: nix-env -iA nixpkgs.delta  or  cargo install git-delta
(use-package magit-delta
  :after magit
  :hook (magit-mode . magit-delta-mode))

;; git-link: copy GitHub/GitLab/etc. URLs for current line or region
;; Docs: https://github.com/sshaw/git-link
;; Bound to C-c v o l (line) and C-c v o L (commit)
(use-package git-link
  :commands (git-link git-link-commit git-link-homepage))

;; treesit-auto: automatically prefer *-ts-mode when a grammar is available
;; Docs: https://github.com/renzmann/treesit-auto
(use-package treesit-auto
  :config (global-treesit-auto-mode))

;; eat: Emulate A Terminal — full ANSI terminal for interactive programs
;; Docs: https://codeberg.org/akib/emacs-eat
;; Integrates with eshell so visual commands (htop, less, man, etc.) open
;; in eat rather than failing
(use-package eat
  :hook (eshell-load . eat-eshell-mode)
  :config (setq eat-kill-buffer-on-exit t))

(provide 'tools-config)
;;; tools.el ends here
