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

## Keep comments terse

This is config, not prose - don't write essays in it.

- Comment only the non-obvious "why" (a gotcha, race, or workaround). Never narrate what the code already says.
- One or two lines is the norm; a multi-line comment paragraph is a smell. Drop backstory, restated mechanics, and "handy for X" asides.
- Prefer a short docstring to a comment block, and never repeat the docstring in a comment above the `defun`.
- If a body seems to need step-by-step explanation, that is a signal to simplify the code, not to add narration.

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
