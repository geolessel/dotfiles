-- Basic settings
vim.opt.number = true         -- line numbers
vim.opt.relativenumber = true -- relative line numbers
vim.opt.cursorline = true     -- highlight current line
vim.opt.wrap = false          -- don't wrap lines
vim.opt.scrolloff = 6         -- keep 6 lines above/below cursor
vim.opt.sidescrolloff = 8     -- keep 8 columns left/right of cursor

-- Indentation
vim.opt.tabstop = 2        -- tab width
vim.opt.shiftwidth = 2     -- indent width
vim.opt.softtabstop = 2    -- soft tab stop
vim.opt.expandtab = true   -- use spaces instead of tab
vim.opt.smartindent = true -- smart auto-indenting
vim.opt.autoindent = true  -- copy indent from current line

-- Search settings
vim.opt.ignorecase = true -- case insensitive search
vim.opt.smartcase = true  -- case sensitive if uppercase in search
vim.opt.hlsearch = false  -- whether to highlight search results
vim.opt.incsearch = true  -- show matches as you type

-- Visual settings
vim.opt.termguicolors = true                      -- enable 24-bit colors
vim.opt.signcolumn = "yes"                        -- always show sign column
vim.opt.colorcolumn = "100"                       -- show column at 100 characters
vim.opt.showmatch = true                          -- highlight matching brackets
vim.opt.matchtime = 2                             -- how long to show matching bracket
vim.opt.cmdheight = 1                             -- command line height
vim.opt.completeopt = "menuone,noinsert,noselect" -- completion options
vim.opt.showmode = false                          -- don't show mode in menu line
vim.opt.pumheight = 10                            -- popup menu height
vim.opt.pumblend = 0                              -- popup menu transparency (0 = opaque)
vim.opt.winblend = 0                              -- floating menu transparency
vim.opt.conceallevel = 0                          -- don't hide markup
vim.opt.lazyredraw = true                         -- don't redraw during macros
vim.opt.synmaxcol = 300                           -- syntax highlighting limit
vim.opt.winborder = "rounded"                     -- rounded borders on little windows

-- File handling
vim.opt.backup = false                            -- don't create backup files
vim.opt.writebackup = false                       -- don't create backup before writing
vim.opt.swapfile = false                          -- don't create swap file
vim.opt.undofile = true                           -- persistent undo
vim.opt.undodir = vim.fn.expand("~/.vim/undodir") -- undo directory
vim.opt.updatetime = 300                          -- faster completion
vim.opt.timeoutlen = 500                          -- key timeout duration
vim.opt.ttimeoutlen = 0                           -- key code timeout
vim.opt.autoread = true                           -- auto reload files changed outside vim
vim.opt.autowrite = false                         -- don't autosave

-- Behaviour settings
vim.opt.hidden = true                   -- allow hidden buffers
vim.opt.errorbells = false              -- no error bells
vim.opt.backspace = "indent,eol,start"  -- better backspace behaviour
vim.opt.autochdir = false               -- don't auto-change directory
vim.opt.iskeyword:append("-")           -- treat dash as part of word
vim.opt.path:append("**")               -- include subdirectories in search
vim.opt.selection = "exclusive"         -- selection behaviour
vim.opt.mouse = "a"                     -- enable mouse support
vim.opt.clipboard:append("unnamedplus") -- use system clipboard
vim.opt.modifiable = true               -- allow buffer modifications
vim.opt.encoding = "UTF-8"              -- set encoding

-- Cursor settings
vim.opt.guicursor =
"n-v-c:block,i-ci-ve:block,r-cr:hor20,o:hor50,a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor,sm:block-blinkwait175-blinkoff150-blinkon175"

-- Command-line completion
vim.opt.wildmenu = true
vim.opt.wildmode = "longest:full,full"

-- Performance improvements
vim.opt.redrawtime = 10000
vim.opt.maxmempattern = 20000

-- Tab display settings
vim.opt.showtabline = 1 -- Always show tabline (0=never, 1=when multiple tabs, 2=always)
vim.opt.tabline = ''    -- Use default tabline (empty string uses built-in)

-- Leader key
vim.g.mapleader = " "      -- set leader key to space
vim.g.maplocalleader = " " -- set local leader key

-- Create undo directory if it doesn't exist
local undodir = vim.fn.expand("~/.vim/undodir")
if vim.fn.isdirectory(undodir) == 0 then
  vim.fn.mkdir(undodir, "p")
end