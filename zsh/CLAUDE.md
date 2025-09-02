# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Zsh Configuration Architecture

This directory contains a modular zsh configuration system with the following key components:

### Core Loading System
- `zshrc`: Main configuration entry point that sources all modules and sets core environment variables
- `functions`: Defines helper functions for loading configs, plugins, and completions
- Configuration files are loaded via `zsh_add_config` function which safely sources files if they exist

### Configuration Modules
- `aliases`: Command aliases organized by category (git, development, file system)
- `options`: Zsh-specific options for history, completion, directories, etc.
- `prompt`: Custom prompt configuration using vcs_info for git integration
- `completion`: Advanced completion system with vim-style navigation
- `bindings`: Key bindings (vi mode is enabled by default)
- `pco`: Additional configuration module

### Envscope System
- `envscope.zsh`: Sophisticated directory-based environment management system
- Automatically loads `.envrc` files with SHA-based security approval
- Provides hierarchical environment management with smart variable tracking
- Commands: `envscope approve <file>`, `envscope clear`, `envscope-status`

### Plugin and Completion System
- `completions/`: Directory for downloaded completion files
- `zsh_add_plugin`: Function to automatically clone and load GitHub plugins
- `zsh_add_completion_file`: Downloads completion files from URLs if missing

## Key Configuration Patterns

### Modular Loading
All configuration uses the pattern:
```bash
zsh_add_config "module_name"  # Sources $ZDOTDIR/module_name if it exists
```

### Environment Variables
- `ZDOTDIR` set to `$HOME/.config/zsh` in main zshrc
- History configuration uses separate file: `$HOME/.zhistory`
- Editor defaults to `nvim`
- Homebrew prefix auto-detected via `brew --prefix`

### Development Workflow
- Vi mode enabled by default (`bindkey -v`)
- Completion system uses vim-style navigation (hjkl keys)
- Git integration in prompt shows branch, staged/unstaged changes, and untracked files
- Aliases focused on git workflow and development tasks

## Important Commands

### Envscope Environment Management
- `envscope approve .envrc`: Approve a previously rejected .envrc file
- `envscope clear`: Clear all hash caches (requires re-approval)
- `envscope-status`: Show active .envrc files and their variables

### Configuration Editing
- `ea`: Edit aliases and reload
- `ee`: Edit environment configuration
- `reload`: Reload aliases configuration

## Testing and Validation

Since this is shell configuration, testing typically involves:
- Source testing: `zsh -c "source ~/.zshrc"`
- Function testing: `zsh -c "source functions; zsh_add_config test"`
- Envscope testing: Create test .envrc files and verify approval/loading workflow