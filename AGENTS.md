# AGENTS.md

Guidance for AI agents changing this repo. Read `README.md` first for the
layout and load order. This file lists the rules and traps.

## What this repo is

This is a Doom Emacs `$DOOMDIR` (non-evil, leader `C-c`). It is deployed
read-only by Nix (fudo-nix-home, flake input `niten-doom-config`), and the
Doom checkout is the latest `doomemacs/doomemacs` with its `sources/doom+`
modules submodule. You can't run Doom here, so be conservative and verify
what you can (see "Validating").

## Rules

1. **Put code in the right place.** New features or languages go in
   `site.d/<topic>.el`. `config.el` holds only global settings. Modules and
   flags go in `init.el`, packages in `packages.el`. If a file for the topic
   already exists, extend it rather than creating a new one.
2. **Don't touch a package's variables or keymaps at top level.** site.d
   files load at startup, before most packages. `(add-to-list 'some-pkg-var …)`,
   `(setf (alist-get … some-pkg-alist) …)` and `(define-key some-pkg-map …)`
   signal `void-variable` and **silently abort the rest of the file** (the
   loader catches the error and only logs it). Wrap them in
   `(after! pkg …)`. Note that the feature can differ from the package name,
   e.g. `eshell-visual-commands` needs `(after! em-term …)`.
3. **Don't make commands depend on a package being loaded.** A command bound
   to a key must be defined at top level, not inside `after!`. Otherwise the
   key is void until something else loads the package. (`hermes.el` and
   `kirocrew.el` don't follow this yet, and that's the only reason
   `config.el` eager-loads `ellama`.)
4. **Don't reimplement what Doom or Emacs provides.** Before writing a
   helper, check:
   - built-ins (`duplicate-line`/`duplicate-dwim`, `comment-line`,
     `kill-matching-lines`, `rename-visited-file`, `delete-horizontal-space`, …)
   - Doom autoloads (`doom/move-this-file`, `doom/copy-this-file`,
     `+default/yank-buffer-path`, `+default/newline-above/below`,
     `+eshell/here`, …)
   - packages Doom already installs (`drag-stuff` on `M-<up>/<down>`,
     `expand-region`, `avy`, `embark`, `org-modern` via `+pretty`,
     `eshell-syntax-highlighting` via `:term eshell`, …)

   In particular, never `defun` a name that already exists, because it
   silently replaces the original.
5. **Packages:** use `maybe-package!` (it skips packages Nix already put on
   `load-path`). Don't declare a package that an enabled Doom module already
   installs: an unpinned `package!` overrides Doom's pin. If Nix should build
   the package too, the fudo-nix-home `defaultEmacsPkgs` list needs a matching
   change (a separate repo, so mention it to the user rather than editing it).
6. **Keybindings:** the leader map sits in `general-override-mode-map` with
   `mode-specific-map` as its parent. A global `C-c <key>` therefore only
   works where `<key>` isn't bound in the leader map. Today that map uses
   `! & a c e f i I l m M n o p q s S t v V w z` (plus `r` if
   `:tools upload` is on). Check `bindings.el` before choosing a key. For keys that only
   apply in one mode, use `:map` or `:localleader` with `map!`, inside or next
   to that mode's config.
7. **No secrets in the repo.** Tokens go in env vars or in
   `$XDG_DATA_HOME/doom/*.el` files (see `hermes.el` and `kirocrew.el` for
   the pattern). `site.d/local-*.el` is gitignored for machine-local overrides.
8. **Match the file conventions:**
   - First line `;;; name.el -*- lexical-binding: t; -*-`, a one-line
     description, then `;;; Code:`
   - End with `(provide '<topic>-config)` and `;;; name.el ends here`
   - Use `use-package` (not `use-package!`, though both work) with `:hook`,
     `:mode`, `:commands` or `:after` so packages load lazily. A bare
     `(use-package foo)` loads `foo` at startup.
   - Personal names get the `my/` prefix (e.g. `my/ellama-chat`). Integration
     modules use their own prefix (`hermes-agent-`, `kirocrew-agent-`,
     `agent-shell-tramp-`). Add a short comment saying *why* a non-obvious
     setting exists.
   - Under `lexical-binding`, `let`-binding another package's variable
     (e.g. `gnutls-verify-error`) only works if it's declared special. Add
     `(defvar that-var)` before the function.
9. **Keep changes minimal and behavior-preserving** unless asked otherwise.
   If you find a bug outside the task, report it instead of widening the
   change.

## Validating

There are no tests. At minimum:

```sh
# 1. parens
for f in *.el site.d/*.el; do
  emacs -Q --batch --eval "(with-temp-buffer (insert-file-contents \"$f\") (emacs-lisp-mode) (check-parens))" || echo "BAD: $f"
done
```

2. Byte-compile the file you touched with Doom macros stubbed, to catch void
   functions/variables and typos:

   ```elisp
   ;; stubs.el
   (require 'cl-lib)
   (defmacro after! (f &rest body) `(with-eval-after-load ',f ,@body))
   (defmacro map! (&rest _) nil)
   (defmacro modulep! (&rest _) nil)
   (defmacro custom-set-faces! (&rest _) nil)
   (defmacro cmd! (&rest _) nil)
   (defvar doom-user-dir default-directory)
   ```

   `emacs -Q --batch -l stubs.el --eval '(byte-compile-file "site.d/foo.el")'`

3. If you can install the packages involved (clone them and pass
   `-L <dir>`), `load` the file in `emacs -Q --batch` with those stubs and
   assert that the variables, hooks and keys you set actually exist. That's
   how the `void-variable` aborts in `avy.el`/`eshell.el` were found.

Doom's own sources are the reference for what's already provided. Clone
`doomemacs/doomemacs` with `--recurse-submodules`; the modules are under
`sources/doom+/modules/` (e.g. `config/default/+emacs-bindings.el`,
`config/default/autoload/*.el`, `term/eshell/config.el`).

## Don't

- Don't edit generated or Doom-owned files, or anything outside this repo.
- Don't add `:ensure t` (Doom ignores it).
- Don't re-copy Doom's `+emacs-bindings.el` into `bindings.el`. Add only the
  bindings that differ.
- Don't reformat or reorder unrelated code.
