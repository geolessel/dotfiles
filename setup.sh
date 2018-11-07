cd $1

ln -s bin/dotfiles/vim .vim
ln -s bin/dotfiles/vim/vimrc .vimrc
mkdir -p .config/nvim
ln -s bin/dotfiles/vim/nvim/init.vim .config/nvim/init.vim

ln -s bin/dotfiles/git_template .git_template
ln -s bin/dotfiles/emacs/emacs .emacs
ln -s bin/dotfiles/emacs/emacs.d .emacs.d
echo "source-file ${1}/bin/dotfiles/tmux.conf" > .tmux.conf
echo "source ${1}/bin/dotfiles/zshrc" > .zshrc

git clone https://github.com/gmarik/Vundle.vim.git ${1}/.vim/bundle/Vundle.vim
echo "Inside vim, run :PluginInstall to install plugins"
