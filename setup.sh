cd $1

### VIM
ln -s usr/dotfiles/vim .vim
ln -s usr/dotfiles/vim/vimrc .vimrc
mkdir -p .config/nvim
ln -s usr/dotfiles/vim/nvim/init.vim .config/nvim/init.vim

### GIT
ln -s usr/dotfiles/git_template .git_template

### EMACS
ln -s usr/dotfiles/emacs/emacs .emacs
ln -s usr/dotfiles/emacs/emacs.d .emacs.d

### TMUX
echo "source-file ${1}/usr/dotfiles/tmux.conf" > .tmux.conf

### ZSH
mkdir -p .config/zsh
echo "export ZDOTDIR=~/.config/zsh" > .zshrc
echo "source ${ZDOTDIR}/zshrc" >> .zshrc
ln -s usr/dotfiles/zsh/p10k.zsh .p10k.zsh

# git clone https://github.com/gmarik/Vundle.vim.git ${1}/.vim/bundle/Vundle.vim
# echo "Inside vim, run :PluginInstall to install plugins"
