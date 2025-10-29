# Emacs Configuration Guide

## Commands
- Byte-compile file: `M-x byte-compile-file`
- Check Emacs Lisp: `M-x check-parens` (to check for balanced parentheses)
- Run Lisp: `M-x eval-buffer` or `M-x eval-region`
- For async evaluation: Add `:async` keyword to code blocks in org-mode

## Code Style Guidelines

### General
- Use lexical binding (set at the top of files)
- Tab width: 2 spaces, no literal tabs
- Max line length: 78 characters
- Always include final newline
- Use UTF-8 encoding

### Elisp
- Prefer `use-package` for package management
- Package initialization pattern:
  ```elisp
  (use-package package-name
    :ensure t
    :init
    (setq var t)  ;; executed BEFORE package load
    :config
    (package-mode 1))  ;; executed AFTER package load
  ```
- Add packages to `load-path` using `user-emacs-directory`
- Keep functionality organized by topic in README.org
- Delete trailing whitespace on save (automatic)

### Naming Conventions
- Personal functions should use `geo/` prefix
- Use descriptive kebab-case names
- Follow Emacs naming conventions:
  - `-p` suffix for predicates
  - `--` prefix for private functions

### Error Handling
- For interactive functions, use `interactive` form
- Wrap file operations in appropriate error handling