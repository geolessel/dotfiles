# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Neovim Configuration Overview

This is a modern Neovim configuration built around Neovim's native plugin management (`vim.pack.add`) rather than external plugin managers. The configuration uses a modular structure with separate files for different concerns.

## Architecture

### Plugin Management
- Uses Neovim's built-in `vim.pack.add()` for plugin installation
- No external plugin manager (no lazy.nvim, packer, etc.)
- Plugins are loaded synchronously during startup
- Each plugin file is self-contained with both declaration and configuration
- Plugin removal is as simple as deleting the corresponding file

### Key Plugin Stack
- **LSP**: `nvim-lspconfig` with built-in Neovim LSP client
- **Completion**: `blink.cmp` for autocompletion
- **File Navigation**: `mini.pick` ecosystem
- **Formatting**: `conform.nvim` with format-on-save
- **Git**: `neogit` + `gitsigns`
- **Treesitter**: `nvim-treesitter` for syntax highlighting

### Configuration Structure
The configuration is organized into logical modules with self-contained plugin files:

```
nvim/
├── init.lua                 # Main entry point (17 lines)
├── lua/config/
│   ├── options.lua         # All vim.opt settings  
│   ├── keymaps.lua         # All keybindings
│   ├── autocmds.lua        # Autocommands
│   └── statusline.lua      # Custom statusline
├── lua/plugins/             # Self-contained plugin modules
│   ├── lsp.lua             # LSP + Mason (declaration + config)
│   ├── completion.lua      # Blink.cmp (declaration + config)
│   ├── picker.lua          # Mini.pick ecosystem (declaration + config)
│   ├── git.lua             # Neogit + Gitsigns (declaration + config)
│   ├── ui.lua              # Mini UI plugins (declaration + config)
│   ├── treesitter.lua      # Treesitter (declaration + config)
│   ├── formatting.lua      # Conform (declaration + config)
│   ├── zig.lua             # Zig language support + ZLS (declaration + config)
│   └── colorscheme.lua     # Catppuccin (declaration + config)
└── lua/utils/
    ├── init.lua           # Utility functions
    └── tabs.lua           # Tab management functions
```

The main `init.lua` loads modules in this order:
1. Core configuration (options, autocmds, statusline)
2. Self-contained plugin modules
3. Keymaps (last to ensure all dependencies are loaded)

### LSP Setup
- Uses `vim.lsp.config()` and `vim.lsp.enable()` (native Neovim LSP)
- Format-on-save enabled globally
- Supports multiple languages via Mason-managed LSP servers
- Language-specific configurations:
  - **Zig**: ZLS with format-on-save for `.zig` and `.zon` files (in `plugins/zig.lua`)

### Key Mappings Philosophy
- Leader key: Space
- Organized by prefix for discoverability
- Uses `mini.clue` for which-key style hints
- Smart file picker: git files in repos, ripgrep elsewhere

## Development Commands

### Plugin Management
- Each plugin file contains both `vim.pack.add{}` declaration and setup
- Plugin files are organized by functionality in `lua/plugins/`
- Restart Neovim to load new plugins
- No separate installation command needed
- Adding a plugin: Create new file in `lua/plugins/` with declaration and config
- Removing a plugin: Delete the corresponding file in `lua/plugins/`

### Testing Configuration
- Edit main config: `<leader>ec`
- Edit specific modules: directly open files in `lua/config/` or `lua/plugins/`
- Reload config: `:source %` or restart Neovim

### File Navigation
Uses `mini.pick` for file operations:
- Smart file picker (git/ripgrep based)
- Live grep functionality
- Help search integration

## Important Notes

### Self-Contained Modular Configuration
Configuration is split across multiple files for maximum maintainability:
- Core settings in `lua/config/`
- Self-contained plugin modules in `lua/plugins/` (each file has both declaration and config)
- Utility functions in `lua/utils/`
- Main entry point remains minimal at 17 lines

### Custom Features
- Custom status line with contextual information (in `config/statusline.lua`)
- Tab management with smart close behavior (in `utils/tabs.lua`)
- Auto-directory creation on file save (in `config/autocmds.lua`)
- Persistent undo configuration (in `config/options.lua`)
- Smart file picker utilities (in `utils/init.lua`)

### Adding New Functionality
- **New plugin**: Create new file in `plugins/` with `vim.pack.add{}` and setup
- **New keybinding**: Add to `config/keymaps.lua`
- **New option**: Add to `config/options.lua`
- **New utility function**: Add to existing or new file in `utils/`

### Plugin File Structure Example
```lua
-- plugins/example.lua
vim.pack.add {
  { src = 'https://github.com/author/plugin-name' },
}

require('plugin-name').setup({
  -- configuration here
})
```

## Important for Claude Code

### Maintaining Documentation
**ALWAYS update this CLAUDE.md file when making changes to the Neovim configuration structure, plugin management approach, or adding/removing significant functionality.** This ensures the documentation stays current and helpful for future interactions.

Examples of changes that require updating this file:
- Changing the modular structure or file organization
- Adding/removing significant plugins or plugin categories
- Modifying the plugin management approach
- Adding new utility modules or major features
- Changing keybinding organization or leader key mappings
