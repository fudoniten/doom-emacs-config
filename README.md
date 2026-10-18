# doom-emacs-config

My [Doom Emacs](https://github.com/doomemacs/doomemacs) private config
(`$DOOMDIR`). The keybindings are Emacs-style (no evil): the leader is `C-c`
and the localleader is `C-c l`.

## How it gets deployed

This repo isn't cloned by hand. [fudo-nix-home](https://github.com/fudoniten/fudo-nix-home)
handles it (`modules/programs/doom-emacs.nix`):

- It is the `niten-doom-config` flake input. home-manager links it read-only
  to `~/.config/doom`, so editing `~/.config/doom` directly won't work.
  Commit here, then bump the input (`nix flake update niten-doom-config`) and
  switch.
- Doom itself is the `doom-emacs` flake input, rsynced to `~/.config/emacs`.
  `doom sync` runs from the Emacs daemon service.
- Most third-party packages are built by Nix (`defaultEmacsPkgs` in that
  module) and are already on `load-path`. That's why `packages.el` uses
  `maybe-package!`, which only declares a package for Doom/straight when Nix
  didn't supply it.
- External tools (aspell, language servers, formatters, `delta`, …) also come
  from Nix (`defaultEmacsDeps`).

## Layout

| Path | Purpose |
|------|---------|
| `init.el` | The `doom!` block: which Doom modules and flags are on. Run `doom sync` after changing it. |
| `packages.el` | Extra packages via `maybe-package!`. Run `doom sync` after changing it. |
| `config.el` | Global settings only: identity, theme and faces, org dirs, persistent `*scratch*`, macOS modifiers. It finishes by calling `load-all-configurations`. |
| `site-functions.el` | The loader (`load-all-configurations`) and its path helpers. |
| `site.d/*.el` | Everything else, one file per feature or language. Loaded alphabetically. |

### `site.d/`

| File | Contents |
|------|----------|
| `agent-shell-tramp.el` | Run agent-shell (ACP) agents on remote hosts over TRAMP. |
| `avy.el` | vi-style verb + noun editing on avy targets (`C-.`, a verb key, the label, then a noun from a hydra menu). The verbs and nouns are two tables at the top of the file. |
| `babashka.el` | `.bb` files as Clojure; CIDER/babashka nREPL. |
| `bindings.el` | Keybindings. It began as a copy of Doom's `+emacs-bindings.el` and is loaded on top of it. |
| `clojure.el` | Clojure: smartparens off (paredit instead), CIDER jack-in keys. |
| `common-lisp.el` | SLY, quicklisp/asdf, localleader keys. |
| `ellama.el` | ellama setup, `ellama-setup-ollama`. |
| `eshell.el` | Eshell commands, env, history, completion. |
| `file-functions.el` | File/buffer helpers. |
| `gptel.el` | gptel with OpenRouter as the default backend. |
| `hermes.el` | Connect gptel/ellama to a Hermes agent (OpenAI-compatible). |
| `kirocrew.el` | Connect gptel/ellama to a Kiro Crew gateway via its local proxy. |
| `lisp.el` | paredit for every Lisp mode, plus its bindings. |
| `nix.el` | nix-ts-mode, eglot + `nil`, nixfmt. |
| `org.el` | Org tweaks. |
| `python.el` | pet (virtualenv detection), ruff via apheleia. |
| `qml.el` | qml-mode (and why it needs `:mode`). |
| `text-functions.el` | Editing helpers (move/copy/open lines, whitespace, …). |
| `tools.el` | diff-hl, magit-delta, git-link, treesit-auto, eat. |
| `tramp.el` | TRAMP performance tuning; Magit/VC caching on remote files. |
| `web.el` | JS/CSS indent, `.less`. |

## Load order

1. Doom loads `init.el`, `packages.el` (at sync time) and the enabled modules.
   That includes `config/default/+emacs-bindings.el`, since
   `(default +bindings)` is on.
2. `config.el` runs top to bottom. Its last form is `(load-all-configurations)`, which:
   1. loads every file in any of `emacs.d`, `site-emacs.d`, `local-emacs.d`
      or `doom.d` under `$XDG_STATE_HOME`, `$XDG_DATA_HOME`,
      `$XDG_RUNTIME_DIR`, `$XDG_CONFIG_HOME` (default `~/.config`) and each
      entry of `$XDG_CONFIG_DIRS`. These are machine-local, untracked
      directories.
   2. loads every file in this repo's `site.d/`, alphabetically.

   Dotfiles, `foo~` backups and `#foo#` autosaves are skipped. An error in one
   file is logged to `*Messages*` (`Error loading file …`) and the remaining
   files still load. **After changing anything, check `*Messages*` for that
   string**, because a broken file otherwise fails silently.

Since `site.d/` loads alphabetically, a file can't rely on another site.d
file having run first unless its name sorts later. Use `after!` (or
`with-eval-after-load`) for anything that touches a package's variables or
keymaps.

## Where to put new code

| You want to… | Put it in |
|---|---|
| Turn a Doom module or flag on or off | `init.el`, then `doom sync` |
| Install a package | `packages.el` with `maybe-package!`. Also add it to `defaultEmacsPkgs` in fudo-nix-home if Nix should build it. Skip this step if an enabled Doom module already installs it. |
| Configure a language or package | The matching `site.d/<topic>.el`, or a new one |
| Add a global keybinding | `site.d/bindings.el` (keys for one mode can sit next to that mode's config, e.g. `lisp.el`, `common-lisp.el`) |
| Add a small editing or file command | `site.d/text-functions.el` / `site.d/file-functions.el`. Check that Emacs or Doom doesn't already have it first. |
| Change a truly global setting (identity, theme, startup) | `config.el` |
| Add machine-specific or secret settings | Outside the repo: an XDG `doom.d`/`site-emacs.d` dir (see above), or `site.d/local-*.el` (gitignored) |

### Secrets and machine-local settings

Never commit tokens. The agent integrations read, in this order: an
environment variable, the `defcustom`, then a local file loaded at startup
that lives outside the repo:

- `$XDG_DATA_HOME/doom/hermes.el`: `hermes-agent-url`, `-model`, `-token`
- `$XDG_DATA_HOME/doom/kirocrew.el`: `kirocrew-agent-url`, `-model`, `-token`

### Environment variables

| Variable | Effect |
|---|---|
| `DOOM_THEME` | Theme name (default `doom-snazzy`) |
| `EMACS_ORG_DIRECTORY` | `org-directory` (default `~/Notes`). `org-roam-directory` is `<that>/roam`. |
| `OPENROUTER_API_KEY` | gptel's default backend |
| `HERMES_AGENT_URL` / `_MODEL` / `_TOKEN` | Hermes agent |
| `KIROCREW_AGENT_URL` / `_MODEL` / `_TOKEN` | Kiro Crew proxy/gateway |

## Keybinding map (non-evil)

- `C-c` is the Doom leader and `C-c l` the localleader. Doom puts the leader
  map in `general-override-mode-map`, so a leader prefix (`C-c c`, `C-c n`,
  `C-c t`, …) **beats a plain `(map! "C-c x" …)` global binding** for the
  same key. Pick global `C-c` keys that aren't leader prefixes.
- `C-c a …` is the AI prefix: agent-shell, gptel (`g`), ellama (`e`),
  hermes (`h`), kirocrew (`k`), aidermacs (`d`).
- `C-c S …` is search (moved from Doom's `C-c s`, which is `consult-line` here).
- `C-c V …` is bookmarks.

## Hacking on it

There's no test suite. For a quick check without a full Doom install:

```sh
# balanced parens in every file
for f in *.el site.d/*.el; do
  emacs -Q --batch --eval "(with-temp-buffer (insert-file-contents \"$f\") (emacs-lisp-mode) (check-parens))" || echo "$f"
done
```

The real test is restarting Emacs (or `M-x doom/reload`) and reading
`*Messages*`.
