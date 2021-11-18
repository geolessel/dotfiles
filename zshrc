source ~/usr/dotfiles/zsh/options
source ~/usr/dotfiles/zsh/prompt
source ~/usr/dotfiles/zsh/aliases
source ~/usr/dotfiles/zsh/env

fpath=(~/usr/dotfiles/zsh/completion $fpath)
autoload -Uz compinit && compinit -i
#source ~/usr/dotfiles/zsh/functions

[[ -d ~/bin ]] && source ~/bin/*.func

# Bring in my bash aliases, env and config
# source ~geo/usr/dotfiles/bash/aliases
# source ~geo/usr/dotfiles/bash/env

# Turn on IEx shell history
export ERL_AFLAGS="-kernel shell_history enabled"
