# Doom Emacs Config

Personal Doom Emacs configuration. Doom wraps many vanilla Emacs APIs with its own macros - always prefer them.

## Doom macros over vanilla

| Instead of | Use |
|---|---|
| `with-eval-after-load`, `eval-after-load` | `after!` |
| `define-key`, `global-set-key`, `evil-define-key*` | `map!` |
| `add-hook` | `add-hook!` |
| `remove-hook` | `remove-hook!` |
| `advice-add` | `defadvice!` |
| `use-package` | `use-package!` |
| `lambda` in keybindings | `cmd!` |
| `require`, `featurep` guards | `after!` - Doom lazy-loads; don't defeat it |

Vanilla `add-hook` with a plain symbol and `advice-add` inside `after!` for pre-existing functions are acceptable.

## Keep inline functions small

`defadvice!` and named `defun` inside `add-hook!` should stay short - a few lines at most. When the function body grows beyond that, move it to the module's `autoload.el` or `autoload/` directory with a `;;;###autoload` cookie (if necessary), and reference it by symbol.

## Module structure

Modules live under `modules/custom/NAME/`:

- `config.el` - settings, hooks, keybindings (loaded at startup)
- `packages.el` - only `package!` forms. No `setq`, no `after!`, no logic.
- `autoload.el` or `autoload/*.el` - `;;;###autoload` before each `defun`/`cl-defun`

After any `packages.el` change, user must run `doom sync`.
Root `init.el` uses `doom!` to declare active modules - don't modify its structure.

## Common mistakes

- `require` or `featurep` - use `after!`
- `provide` in config files - only in library files under `lisp/`
- `:ensure t` in `use-package!` - Doom manages packages via `packages.el`
- `M-x package-install` - use `packages.el` + `doom sync`
- `pushnew!`, `appendq!` - obsolete since Doom 2.1; use `cl-pushnew` or `add-to-list`
- Missing `;;;###autoload` on functions in `autoload/` dirs
- Anonymous lambdas in `add-hook!` - use named `defun`
