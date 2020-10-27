source ~/usr/dotfiles/zsh/options
source ~/usr/dotfiles/zsh/prompt
source ~/usr/dotfiles/zsh/aliases
fpath=(~/usr/dotfiles/zsh/completion $fpath)
autoload -Uz compinit && compinit -i
#source ~/usr/dotfiles/zsh/functions

# Bring in my bash aliases, env and config
# source ~geo/usr/dotfiles/bash/aliases
# source ~geo/usr/dotfiles/bash/env
