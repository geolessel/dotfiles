cd $1
ln -s bin/dotfiles/vim .vim
ln -s bin/dotfiles/vim/vimrc .vimrc
echo "source-file ${1}/bin/dotfiles/tmux.conf" > .tmux.conf
echo "source ${1}/bin/dotfiles/zshrc" > .zshrc
