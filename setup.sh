#!/usr/bin/env bash
# Links these dotfiles into a home folder: ./setup.sh [home]  (default: $HOME)
# Expects this repo at <home>/usr/dotfiles. Safe to run again: a correct link
# is left alone, and an existing file or folder is never replaced. Anything
# skipped is listed at the end and makes the script exit 1.
set -euo pipefail

cd "${1:-$HOME}"
DOTFILES="$PWD/usr/dotfiles"
[ -d "$DOTFILES" ] || { echo "error: $DOTFILES not found" >&2; exit 1; }

skipped=()

# link <path in dotfiles> <path in home>
link() {
  local src="$DOTFILES/$1" dest="$2"
  if [ ! -e "$src" ]; then
    skipped+=("$dest (missing source $src)")
    return
  fi
  mkdir -p "$(dirname "$dest")"
  if [ -L "$dest" ] && [ "$(realpath "$dest" 2>/dev/null)" = "$(realpath "$src")" ]; then
    echo "ok      $dest"
  elif [ -e "$dest" ] || [ -L "$dest" ]; then
    skipped+=("$dest (already exists)")
  else
    ln -s "$src" "$dest"
    echo "linked  $dest -> $src"
  fi
}

### ZSH
# zsh reads ZDOTDIR from ~/.zshenv before it looks for .zshrc.
if [ -e .zshenv ]; then
  grep -q 'ZDOTDIR=' .zshenv && echo "ok      .zshenv" || skipped+=(".zshenv (exists without ZDOTDIR)")
else
  cat > .zshenv <<'EOF'
export ZDOTDIR="$HOME/.config/zsh"
export XDG_CONFIG_HOME="$HOME/.config"
export DISABLE_DEVBOX_DIRENV=true
export PATH="$PATH:/opt/homebrew/bin"
EOF
  echo "wrote   .zshenv"
fi
link zsh .config/zsh

### TMUX
link tmux .config/tmux
CATPPUCCIN="$DOTFILES/tmux/plugins/catppuccin/tmux"
if [ -d "$CATPPUCCIN" ]; then
  echo "ok      $CATPPUCCIN"
else
  git clone -q -c advice.detachedHead=false -b v2.1.3 https://github.com/catppuccin/tmux.git "$CATPPUCCIN"
  echo "cloned  $CATPPUCCIN"
fi

### EDITORS
link nvim .config/nvim
link vim .vim
link vim/vimrc .vimrc
link emacs/emacs.d .emacs.d

### TERMINALS AND TOOLS
link ghostty .config/ghostty
link mise .config/mise
link herdr .config/herdr
link herdr-app .config/herdr-app

### CLAUDE
# settings.json is not linked: Claude Code rewrites it in place.
link agents/claude/CLAUDE.md .claude/CLAUDE.md
link agents/claude/commands .claude/commands
link agents/claude/templates .claude/templates

if [ ${#skipped[@]} -gt 0 ]; then
  echo
  echo "Skipped (fix by hand, then run again):" >&2
  printf '  %s\n' "${skipped[@]}" >&2
  exit 1
fi
