;;; qml.el -*- lexical-binding: t; -*-
;; QML (Qt Declarative) configuration

;;; Code:

;; qml-mode ships from Nix (see `defaultEmacsPkgs' in fudo-nix-home's
;; modules/programs/doom-emacs.nix), so `packages.el' deliberately skips the
;; `package!' declaration for it. That puts qml-mode.el on `load-path', but
;; nothing loads qml-mode-autoloads.el: the Nix Emacs wrapper only extends
;; `load-path' (via its generated subdirs.el), and Doom sets
;; `package-enable-at-startup' to nil, so package.el never activates the ELPA
;; tree Nix built. Everything qml-mode needs to announce itself -- the
;; `qml-mode' command and its `auto-mode-alist' entry -- lives behind those
;; autoload cookies, so .qml files landed in fundamental-mode.
;;
;; Declaring `:mode' here makes use-package register the entry and the autoload
;; stub itself, which is the same thing nix.el does for nix-ts-mode.
(use-package qml-mode
  :mode "\\.qml\\'")

(provide 'qml-config)
;;; qml.el ends here
