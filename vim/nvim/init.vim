" Basic settings
set number                 " Show line numbers
set relativenumber         " Relative line numbers
set clipboard=unnamedplus  " Use system clipboard
set tabstop=2              " Number of spaces in a tab
set shiftwidth=2           " Number of spaces for indentation
set expandtab              " Convert tabs to spaces

call plug#begin('~/.local/share/nvim/plugged')

" Elixir syntax highlighting plugin
Plug 'elixir-editors/vim-elixir'

call plug#end()

"""""""" FZF
" Homebrew on Mac Silicon
set rtp+=/opt/homebrew/opt/fzf
" Map Ctrl-p to the custom FZF function
nnoremap <silent> <C-p> :call fzf#run({'sink': 'e'})<CR>
