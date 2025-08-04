-- theme & transparency
vim.cmd.colorscheme("unokai")

-- Basic settings
vim.opt.number = true         -- line numbers
vim.opt.relativenumber = true -- relative line numbers
vim.opt.cursorline = true     -- highlight current line
vim.opt.wrap = false          -- don't wrap lines
vim.opt.scrolloff = 6         -- keep 10 lines above/below cursor
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

-- Auto-resize splits when window is resized
vim.api.nvim_create_autocmd("VimResized", {
  callback = function()
    vim.cmd("wincmd =")
  end,
})

--- ==================================================================================
--- Plugins
--- ==================================================================================

vim.pack.add {
  { src = 'https://github.com/neovim/nvim-lspconfig' },
  { src = 'https://github.com/Saghen/blink.cmp' },       -- blink for autocompletion
  { src = 'https://github.com/echasnovski/mini.pick' },  -- mini picker for file finding, etc
  { src = 'https://github.com/echasnovski/mini.icons' }, -- icons for mini picker
  { src = 'https://github.com/stevearc/conform.nvim' },  -- better code formatting
  -- TELESCOPE stuff
  { src = 'https://github.com/nvim-lua/plenary.nvim' },  -- required by telescope
  { src = 'https://github.com/nvim-telescope/telescope-fzf-native.nvim' },
  { src = 'https://github.com/nvim-telescope/telescope.nvim' },
  { src = 'https://github.com/nvim-treesitter/nvim-treesitter' },
  -- end TELESCOPE
  { src = 'https://github.com/folke/which-key.nvim' },
  { src = 'https://github.com/mason-org/mason.nvim' }, -- easy(er) LSP server management
}

require("mason").setup()
require('mini.pick').setup()
require('mini.icons').setup()

-- Treesitter configuration
require 'nvim-treesitter.configs'.setup {
  ensure_installed = { "elixir", "heex", "eex", "lua", "typescript", "tsx", "javascript" },
  auto_install = true,
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  },
  indent = {
    enable = true,
  },
}

require('blink.cmp').setup({
  -- 'default' (recommended) for mappings similar to built-in completions (C-y to accept)
  -- 'super-tab' for mappings similar to vscode (tab to accept)
  -- 'enter' for enter to accept
  -- 'none' for no mappings
  --
  -- All presets have the following mappings:
  -- C-space: Open menu or open docs if already open
  -- C-n/C-p or Up/Down: Select next/previous item
  -- C-e: Hide menu
  -- C-k: Toggle signature help (if signature.enabled = true)
  --
  -- See :h blink-cmp-config-keymap for defining your own keymap
  keymap = { preset = 'default' },

  appearance = {
    -- 'mono' (default) for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
    -- Adjusts spacing to ensure icons are aligned
    nerd_font_variant = 'mono'
  },

  -- (Default) Only show the documentation popup when manually triggered
  completion = { documentation = { auto_show = false } },

  -- Default list of enabled providers defined so that you can extend it
  -- elsewhere in your config, without redefining it, due to `opts_extend`
  sources = {
    default = { 'lsp', 'path', 'snippets', 'buffer' },
  },

  -- (Default) Rust fuzzy matcher for typo resistance and significantly better performance
  -- You may use a lua implementation instead by using `implementation = "lua"` or fallback to the lua implementation,
  -- when the Rust fuzzy matcher is not available, by using `implementation = "prefer_rust"`
  --
  -- See the fuzzy documentation for more information
  fuzzy = { implementation = "prefer_rust_with_warning" }
})

require("conform").setup({
  formatters_by_ft = {
    -- lua = { "stylua" },
    -- Conform will run multiple formatters sequentially
    -- python = { "isort", "black" },
    -- You can customize some of the format options for the filetype (:help conform.format)
    rust = { "rustfmt", lsp_format = "fallback" },
    -- Conform will run the first available formatter
    javascript = { "prettierd", "prettier", "eslint_d", stop_after_first = true },
    typescript = { "prettierd", "prettier", "eslint_d", stop_after_first = true },
    javascriptreact = { "prettierd", "prettier", "eslint_d", stop_after_first = true },
    typescriptreact = { "prettierd", "prettier", "eslint_d", stop_after_first = true },
  },
  format_on_save = {
    -- These options will be passed to conform.format()
    timeout_ms = 3000,
    lsp_format = "fallback",
  },
})


--- ==================================================================================
--- KEYMAPS
--- ==================================================================================

-- Center screen when jumping
-- vim.keymap.set("n", "n", "nzzzv", { desc = "Next search result (centered)" })
-- vim.keymap.set("n", "N", "Nzzzv", { desc = "Previous search result (centered)" })
vim.keymap.set("n", "<C-d>", "<C-d>zz", { desc = "Half page down (centered)" })
vim.keymap.set("n", "<C-u>", "<C-u>zz", { desc = "Half page up (centered)" })
vim.keymap.set("n", "<C-l>", "zz", { desc = "Vertically center cursor in window" })

-- Leader key
vim.g.mapleader = " "      -- set leader key to space
vim.g.maplocalleader = " " -- set local leader key

-- Buffer navigation
vim.keymap.set("n", "<leader>bn", ":bnext<CR>", { desc = "Next buffer" })
vim.keymap.set("n", "<leader>bp", ":bprevious<CR>", { desc = "Previous buffer" })

-- Window management
vim.keymap.set("n", "<leader>wv", ":vsplit<CR>", { desc = "Split window vertically" })
vim.keymap.set("n", "<leader>ws", ":split<CR>", { desc = "Split window horizontally" })
vim.keymap.set("n", "<C-Up>", ":resize +2<CR>", { desc = "Increase window height" })
vim.keymap.set("n", "<C-Down>", ":resize -2<CR>", { desc = "Reduce window height" })
vim.keymap.set("n", "<C-Right>", ":vertical resize +2<CR>", { desc = "Increase window width" })

-- Window navigation
-- vim.keymap.set("n", "<C-h>", "<C-w>h", { desc = "Move to left window" })
-- vim.keymap.set("n", "<C-j>", "<C-w>j", { desc = "Move to bottom window" })
-- vim.keymap.set("n", "<C-k>", "<C-w>k", { desc = "Move to top window" })
-- vim.keymap.set("n", "<C-l>", "<C-w>l", { desc = "Move to right window" })

-- Move lines up/down
vim.keymap.set("n", "<A-j>", ":m .+1<CR>==", { desc = "Move line down" })
vim.keymap.set("n", "<A-k>", ":m .-2<CR>==", { desc = "Move line up" })
vim.keymap.set("v", "<A-j>", ":m '>+1<CR>gv=gv", { desc = "Move selection down" })
vim.keymap.set("v", "<A-k>", ":m '<-2<CR>gv=gv", { desc = "Move selection up" })

-- Keep select while indenting
vim.keymap.set("v", "<", "<gv", { desc = "Indent left and reselect" })
vim.keymap.set("v", ">", ">gv", { desc = "Indent right and reselect" })

-- Quick config editing
vim.keymap.set("n", "<leader>ec", ":e ~/.config/nvim/init.lua<CR>", { desc = "Edit config" })

-- Buffer management
vim.keymap.set("n", "<leader>bf", vim.lsp.buf.format, { desc = "Format buffer with LSP" })
vim.keymap.set("n", "<leader>bb", MiniPick.builtin.buffers, { desc = "Pick open buffer" })
vim.keymap.set("n", "<C-p>", MiniPick.builtin.files, { desc = "Open file picker" })

--- ==================================================================================
--- Utilities
--- ==================================================================================

local undodir = vim.fn.expand("~/.vim/undodir")
if vim.fn.isdirectory(undodir) == 0 then
  vim.fn.mkdir(undodir, "p")
end

--- ==================================================================================
--- Status line
--- ==================================================================================

-- git branch function
local function git_branch()
  local branch = vim.fn.system("git branch --show-current 2>/dev/null | tr -d '\n'")
  if branch ~= "" then
    return branch
  end
  return ""
end

-- LSP status
local function lsp_status()
  local clients = vim.lsp.get_clients({ bufnr = 0 })
  if #clients > 0 then
    return "  LSP "
  end
  return ""
end

-- Word count for text files
local function word_count()
  local ft = vim.bo.filetype
  if ft == "markdown" or ft == "text" or ft == "tex" then
    local words = vim.fn.wordcount().words
    return "  " .. words .. " words "
  end
  return ""
end

-- Mode indicators with icons
local function mode_icon()
  local mode = vim.fn.mode()
  local modes = {
    n = "NORMAL",
    i = "INSERT",
    v = "VISUAL",
    V = "V-LINE",
    ["\22"] = "V-BLOCK", -- Ctrl-V
    c = "COMMAND",
    s = "SELECT",
    S = "S-LINE",
    ["\19"] = "S-BLOCK", -- Ctrl-S
    R = "REPLACE",
    r = "REPLACE",
    ["!"] = "SHELL",
    t = "TERMINAL"
  }
  return modes[mode] or "  " .. mode:upper()
end

-- make these global
_G.mode_icon = mode_icon
_G.git_branch = git_branch
_G.lsp_status = lsp_status

vim.cmd([[
  highlight StatusLineBold gui=bold cterm=bold
]])

-- Function to change statusline based on window focus
local function setup_dynamic_statusline()
  vim.api.nvim_create_autocmd({ "WinEnter", "BufEnter" }, {
    callback = function()
      vim.opt_local.statusline = table.concat {
        "%#StatusLineBold#",
        "%{v:lua.mode_icon()}",
        "%#StatusLine#",
        " %f %h%m%r %l:%c %P", -- Line:Column Percentage
        "%=",                  -- Right-align everything after this
        "%{v:lua.lsp_status()}",
        " | ⎇  %{v:lua.git_branch()} ",
      }
    end
  })
  vim.api.nvim_set_hl(0, "StatusLineBold", { bold = true, bg = "NONE" })
  vim.api.nvim_set_hl(0, "StatusLine", { bg = "NONE" })

  vim.api.nvim_create_autocmd({ "WinLeave", "BufLeave" }, {
    callback = function()
      vim.opt_local.statusline = "%f %h%m%r %l:%c %P "
    end
  })
end

setup_dynamic_statusline()


-- ============================================================================
-- TABS
-- ============================================================================

-- Tab display settings
vim.opt.showtabline = 1 -- Always show tabline (0=never, 1=when multiple tabs, 2=always)
vim.opt.tabline = ''    -- Use default tabline (empty string uses built-in)

-- Transparent tabline appearance
vim.cmd([[
  hi TabLineFill guibg=NONE ctermfg=242 ctermbg=NONE
]])

-- Alternative navigation (more intuitive)
vim.keymap.set('n', '<leader>tn', ':tabnew<CR>', { desc = 'New tab' })
vim.keymap.set('n', '<leader>tc', ':tabclose<CR>', { desc = 'Close tab' })

-- Tab moving
vim.keymap.set('n', '<leader>tm', ':tabmove<CR>', { desc = 'Move tab' })
vim.keymap.set('n', '<leader>t>', ':tabmove +1<CR>', { desc = 'Move tab right' })
vim.keymap.set('n', '<leader>t<', ':tabmove -1<CR>', { desc = 'Move tab left' })

-- Function to open file in new tab
local function open_file_in_tab()
  vim.ui.input({ prompt = 'File to open in new tab: ', completion = 'file' }, function(input)
    if input and input ~= '' then
      vim.cmd('tabnew ' .. input)
    end
  end)
end

-- Function to duplicate current tab
local function duplicate_tab()
  local current_file = vim.fn.expand('%:p')
  if current_file ~= '' then
    vim.cmd('tabnew ' .. current_file)
  else
    vim.cmd('tabnew')
  end
end

-- Function to close tabs to the right
local function close_tabs_right()
  local current_tab = vim.fn.tabpagenr()
  local last_tab = vim.fn.tabpagenr('$')

  for i = last_tab, current_tab + 1, -1 do
    vim.cmd(i .. 'tabclose')
  end
end

-- Function to close tabs to the left
local function close_tabs_left()
  local current_tab = vim.fn.tabpagenr()

  for i = current_tab - 1, 1, -1 do
    vim.cmd('1tabclose')
  end
end

-- Enhanced keybindings
vim.keymap.set('n', '<leader>tO', open_file_in_tab, { desc = 'Open file in new tab' })
vim.keymap.set('n', '<leader>td', duplicate_tab, { desc = 'Duplicate current tab' })
vim.keymap.set('n', '<leader>tr', close_tabs_right, { desc = 'Close tabs to the right' })
vim.keymap.set('n', '<leader>tL', close_tabs_left, { desc = 'Close tabs to the left' })

-- Function to close buffer but keep tab if it's the only buffer in tab
local function smart_close_buffer()
  local buffers_in_tab = #vim.fn.tabpagebuflist()
  if buffers_in_tab > 1 then
    vim.cmd('bdelete')
  else
    -- If it's the only buffer in tab, close the tab
    vim.cmd('tabclose')
  end
end
vim.keymap.set('n', '<leader>bd', smart_close_buffer, { desc = 'Smart close buffer/tab' })


--- ==================================================================================
--- LSP and treesitter
--- ==================================================================================

vim.lsp.config('elixirls', {
  cmd = { '/Users/geo/.local/share/nvim/mason/packages/elixir-ls/language_server.sh' },
})

-- get rid of the vim warnings in lua files
vim.lsp.config('lua_ls', {
  settings = {
    Lua = {
      workspace = {
        library = vim.api.nvim_get_runtime_file("", true),
      }
    }
  }
})

vim.lsp.enable({ 'lua_ls', 'elixirls', 'ts_ls' })

-- format on save
vim.api.nvim_create_autocmd("BufWritePre", {
  callback = function()
    vim.lsp.buf.format()
  end,
})

-- Configure LSP floating windows with borders
vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
  border = "rounded"
})

vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
  border = "rounded"
})

-- Configure diagnostic floating windows
vim.diagnostic.config({
  float = {
    border = "rounded"
  }
})

-- LSP floating window colors to match background
vim.cmd([[
  highlight NormalFloat guibg=NONE ctermbg=NONE
  highlight FloatBorder guibg=NONE ctermbg=NONE
]])

-- -- From the LSP help file; trigger on .
-- local triggers = { '.' }
-- vim.api.nvim_create_autocmd('InsertCharPre', {
--   buffer = vim.api.nvim_get_current_buf(),
--   callback = function()
--     if vim.fn.pumvisible() == 1 or vim.fn.state('m') == 'm' then
--       return
--     end
--     local char = vim.v.char
--     if vim.list_contains(triggers, char) then
--       local key = vim.keycode('<C-x><C-n>')
--       vim.api.nvim_feedkeys(key, 'm', false)
--     end
--   end
-- })


-- --- ==================================================================================
-- --- Floating terminal
-- --- ==================================================================================
--
-- -- terminal
-- local terminal_state = {
--   buf = nil,
--   win = nil,
--   is_open = false
-- }
--
-- local function FloatingTerminal()
--   -- if terminal is already open, close it (toggle behaviour)
--   if terminal_state.is_open and vim.api.nvim_win_is_valid(terminal_state.win) then
--     vim.api.nvim_win_close(terminal-state.win, false)
--     terminal_state.is_open = false
--     return
--   end
--
--   -- create buffer if it doesn't exist or is invalid
--   if not terminal_state.buf or not vim.api.nvim_buf_is_valid(terminal_state.buf) then
--     terminal_state.buf = vim.api.nvim_create_buf(false, true)
--     -- set buffer options for better terminal experience
--     vim.api.nvim_buf_set_option(terminal_state.buf, 'bufhidden', 'hide')
--   end
--
--   -- calculate window dimensions
--   local width = math.floor(vim.o.columns * 0.8)
--   local height = math.floor(vim.o.lines * 0.8)
--   local row = math.floor((vim.o.lines - height) / 2)
--   local col = math.floor((vim.o.columns - width) / 2)
--
--   -- create the floating window
--   terminal_state_win = vim.api.nvim_open_win(terminal_state.buf, true, {
--     relative = 'editor',
--     width = width,
--     height = height,
--     row = row,
--     col = col,
--     style = 'minimal',
--     border = 'rounded',
--   })
--
--   -- start terminal if not already running
--   local has_terminal = false
--   local lines = vim.api.nvim_buf_get_lines(terminal_state.buf, 0, -1, false)
--   for _, line in ipairs(lines) do
--     if line ~= "" then
--       has_terminal = true
--       break
--     end
--   end
--
--   if not has_terminal then
--     vim.fn.termopen(os.getenv("SHELL"))
--   end
--
--   terminal_state.is_open = true
--   vim.cmd("startinsert")
--
--   -- set up auto-close on buffer leave
--   vim.api.nvim_create_autocmd("BufLeave", {
--     buffer = terminal_state.buf,
--     callback = function()
--       if terminal_state.is_open and vim.api.nvim_win_is_valid(terminal_state.win) then
--         vim.api.nvim_win_close(terminal_state.win, false)
--         terminal_state.is_open = false
--       end
--     end,
--     once = true
--   })
-- end
--
-- -- function to explicitly close the floating terminal
-- local function CloseFloatingTerminal()
--   if terminal_state.is_open and vim.api.nvim_win_is_valid(terminal_state.win) then
--     vim.api.nvim_win_close(terminal_state.win, false)
--     terminal_state.is_open = false
--   end
-- end
--
-- -- key mappings
-- vim.keymap.set("n", "<leader>t", FloatingTerminal, { noremap = true, silent = true, desc = "Toggle floating terminal" })
-- vim.keymap.set("t", "<Esc>", function()
--   if terminal_state.is_open then
--     vim.api.nvim_win_close(terminal_state.win, false)
--     terminal_state.is_open = false
--   end
-- end, { noremap = true, silent = true, desc = "Close floating terminal from terminal mode" })
