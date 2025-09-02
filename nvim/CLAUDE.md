# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Neovim Configuration Overview

This is a modern Neovim configuration built around Neovim's native plugin management (`vim.pack.add`) rather than external plugin managers. The configuration is entirely contained in a single `init.lua` file.

## Architecture

### Plugin Management
- Uses Neovim's built-in `vim.pack.add()` for plugin installation
- No external plugin manager (no lazy.nvim, packer, etc.)
- Plugins are loaded synchronously during startup

### Key Plugin Stack
- **LSP**: `nvim-lspconfig` with built-in Neovim LSP client
- **Completion**: `blink.cmp` for autocompletion
- **File Navigation**: `mini.pick` ecosystem
- **Formatting**: `conform.nvim` with format-on-save
- **Git**: `neogit` + `gitsigns`
- **Treesitter**: `nvim-treesitter` for syntax highlighting

### Configuration Structure
The `init.lua` file is organized in these sections:
1. Basic Vim settings
2. Plugin definitions and setup
3. Colorscheme
4. Keymaps
5. Custom status line
6. Tab management
7. LSP configuration

### LSP Setup
- Uses `vim.lsp.config()` and `vim.lsp.enable()` (native Neovim LSP)
- Format-on-save enabled globally
- Supports multiple languages via Mason-managed LSP servers

### Key Mappings Philosophy
- Leader key: Space
- Organized by prefix for discoverability
- Uses `mini.clue` for which-key style hints
- Smart file picker: git files in repos, ripgrep elsewhere

## Development Commands

### Plugin Management
- Plugins are declared in `vim.pack.add{}` block
- Restart Neovim to load new plugins
- No separate installation command needed

### Testing Configuration
- Edit config: `<leader>ec`
- Reload config: `:source %` or restart Neovim

### File Navigation
Uses `mini.pick` for file operations:
- Smart file picker (git/ripgrep based)
- Live grep functionality
- Help search integration

## Important Notes

### Single File Configuration
All configuration is in `init.lua` - no modular setup with separate files.

### Custom Features
- Custom status line with contextual information
- Tab management with smart close behavior
- Auto-directory creation on file save
- Persistent undo configuration