# Geo's Dotfiles

My dotfiles I use on my development machines.
Not all the files and configurations are used all the time.
Over time, tools fall in and out of favor and their configs may be left to rust away.
A good example of this is my journey from vi -> vim -> nvim -> emacs -> nvim.

## Overview

This repository contains configuration files for various development tools and applications:

- **Shell**: Bash and Zsh configurations with custom prompts and aliases
- **Editors**: 
  - Neovim (modern, modular configuration with native plugin management)
  - Vim (traditional setup)
  - Emacs (comprehensive configuration)
- **Development Tools**: Git templates, tmux, and custom utilities
- **macOS Tools**: Hammerspoon automation and Karabiner keyboard customization
- **Custom Scripts**: Utility scripts and binaries in `bin/` and `src/`

## Structure

- `bash/`, `zsh/` - Shell configurations
- `nvim/`, `vim/`, `emacs/` - Editor configurations  
- `git_template/` - Git hooks and templates
- `hammerspoon/`, `karabiner/` - macOS automation and keyboard tools
- `bin/` - Custom utility scripts
- `src/` - Source code for custom tools

## Features

- **Cross-shell compatibility** with both Bash and Zsh support
- **Modern Neovim setup** using native plugin management and modular architecture
- **Custom utilities** for development workflow enhancement
- **macOS integration** with system automation tools
- **Version controlled** configuration with easy rollback capabilities

## Notes

Each major tool has its own `CLAUDE.md` file containing detailed configuration information and development guidelines for AI-assisted maintenance.

---

## GPG Setup for Commit Signing

1. Install GPGTools - [https://gpgtools.org/](https://gpgtools.org/)

2. Transfer old or create new GPG keys

3. Configure git to look for these

    ```
    git config --global gpg.program /usr/local/MacGPG2/bin/gpg2
    git config --global commit.gpgsign true
    ```

4. Add this to ~/.gnupg/gpg-agent.conf

    ```
    pinentry-program /usr/local/MacGPG2/libexec/pinentry-mac.app/Contents/MacOS/pinentry-mac
    ```

5. Add this to `~/.gnupg/gpg.conf`
    ```
    no-tty
    ```
