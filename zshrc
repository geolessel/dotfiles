source ~/bin/dotfiles/zsh/options
source ~/bin/dotfiles/zsh/prompt
source ~/bin/dotfiles/zsh/aliases
fpath=(~/bin/dotfiles/zsh/completion $fpath)
autoload -Uz compinit && compinit -i
#source ~/bin/dotfiles/zsh/functions

# Bring in my bash aliases, env and config
# source ~geo/bin/dotfiles/bash/aliases
# source ~geo/bin/dotfiles/bash/env
