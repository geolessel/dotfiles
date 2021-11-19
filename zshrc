# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ -f $(brew --prefix)/opt/powerlevel10k/powerlevel10k.zsh-theme ]] && source $(brew --prefix)/opt/powerlevel10k/powerlevel10k.zsh-theme
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

source ~/usr/dotfiles/zsh/options
source ~/usr/dotfiles/zsh/prompt
source ~/usr/dotfiles/zsh/aliases
source ~/usr/dotfiles/zsh/env
source ~/usr/dotfiles/zsh/pco

### NVM ###
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
###########

fpath=(~/usr/dotfiles/zsh/completion $fpath)
autoload -Uz compinit && compinit -i
#source ~/usr/dotfiles/zsh/functions

[[ -d ~/bin ]] && source ~/bin/*.func

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# to get vterm to work in Emacs
vterm_printf(){
  if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
    # Tell tmux to pass the escape sequences through
    printf "\ePtmux;\e\e]%s\007\e\\" "$1"
  elif [ "${TERM%%-*}" = "screen" ]; then
    # GNU screen (screen, screen-256color, screen-256color-bce)
    printf "\eP\e]%s\007\e\\" "$1"
  else
    printf "\e]%s\e\\" "$1"
  fi
}
