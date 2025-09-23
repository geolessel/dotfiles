local utils = require('utils')
local tabs = require('utils.tabs')

-- Center screen when jumping
vim.keymap.set("n", "<C-d>", "<C-d>zz", { desc = "Half page down (centered)" })
vim.keymap.set("n", "<C-u>", "<C-u>zz", { desc = "Half page up (centered)" })
vim.keymap.set("n", "<C-l>", "zz", { desc = "Vertically center cursor in window" })

-- Window management
vim.keymap.set("n", "<leader>wv", ":vsplit<CR>", { desc = "Split window vertically" })
vim.keymap.set("n", "<leader>ws", ":split<CR>", { desc = "Split window horizontally" })
vim.keymap.set("n", "<C-Up>", ":resize +2<CR>", { desc = "Increase window height" })
vim.keymap.set("n", "<C-Down>", ":resize -2<CR>", { desc = "Reduce window height" })
vim.keymap.set("n", "<C-Right>", ":vertical resize +2<CR>", { desc = "Increase window width" })

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
vim.keymap.set("n", "<leader>bn", ":bnext<CR>", { desc = "Next buffer" })
vim.keymap.set("n", "<leader>bp", ":bprevious<CR>", { desc = "Previous buffer" })
vim.keymap.set("n", "<leader>bF", vim.lsp.buf.format, { desc = "Format buffer with LSP" })
vim.keymap.set("n", "<leader>bb", MiniPick.builtin.buffers, { desc = "Pick open buffer" })
vim.keymap.set('n', '<leader>bd', tabs.smart_close_buffer, { desc = 'Smart close buffer/tab' })

-- Delete current file and close buffer
vim.keymap.set('n', '<leader>bfd', function()
  local file = vim.api.nvim_buf_get_name(0)
  if file == '' then
    print("No file to delete")
    return
  end

  local choice = vim.fn.confirm(
    string.format("Delete file '%s'?", file),
    "&Yes\n&No",
    2
  )

  if choice == 1 then
    vim.fn.delete(file)
    vim.cmd('bdelete!')
    print("File deleted: " .. vim.fs.basename(file))
  end
end, { desc = 'Delete file and close buffer' })

-- File picker
vim.keymap.set("n", "<C-p>", utils.smart_file_picker, { desc = "Open file picker" })

-- Searching
vim.keymap.set("n", "<C-_>", MiniPick.builtin.grep_live, { desc = "Recursive grep" })
-- Ripgrep to quickfix list
vim.keymap.set("n", "<leader>sg", function()
  vim.ui.input({ prompt = 'Ripgrep pattern: ' }, function(pattern)
    if pattern and pattern ~= '' then
      vim.cmd('cgetexpr system("rg --vimgrep \\"' .. pattern .. '\\"") | copen')
    end
  end)
end, { desc = "Ripgrep to quickfix list" })

-- Help
vim.keymap.set("n", "<C-h>", MiniPick.builtin.help, { desc = "Find help" })

-- Git (neogit)
vim.keymap.set("n", "<leader>gg", ":Neogit cwd=%:p:h<CR>", { desc = "Open Neogit" })
vim.keymap.set("n", "<leader>gb", ":Gitsigns blame<CR>", { desc = "git blame" })
vim.keymap.set("n", "<leader>gB", ":Gitsigns blame_line<CR>", { desc = "git blame on current line" })
vim.keymap.set("n", "<leader>gtb", ":Gitsigns toggle_current_line_blame<CR>", { desc = "toggle current line blame" })
vim.keymap.set("n", "<leader>gts", ":Gitsigns toggle_signs<CR>", { desc = "toggle git signs" })
vim.keymap.set("n", "<leader>gtn", ":Gitsigns toggle_numhl<CR>", { desc = "toggle git line number highlight" })
vim.keymap.set("n", "<leader>gtl", ":Gitsigns toggle_linehl<CR>", { desc = "toggle git line highlight" })
vim.keymap.set("n", "<leader>gtw", ":Gitsigns toggle_word_diff<CR>", { desc = "toggle git word diff" })

-- Preferences
vim.keymap.set("n", "<leader>,c", ":Pick colorschems<CR>", { desc = "Colorscheme" })
vim.keymap.set("n", "<leader>,i", function()
  if vim.g.miniindentscope_disable then
    vim.g.miniindentscope_disable = false
    print("mini.indentscope enabled")
  else
    vim.g.miniindentscope_disable = true
    print("mini.indentscope disabled")
  end
end, { desc = "Toggle indentscope" })
vim.keymap.set("n", "<leader>,r", function()
  vim.opt.relativenumber = not vim.opt.relativenumber:get()
  if vim.opt.relativenumber:get() then
    print("Relative line numbers enabled")
  else
    print("Relative line numbers disabled")
  end
end, { desc = "Toggle relative numbers" })
vim.keymap.set("n", "<leader>,n", function()
  vim.opt.number = not vim.opt.number:get()
  if vim.opt.number:get() then
    print("Line numbers enabled")
  else
    print("Line numbers disabled")
  end
end, { desc = "Toggle line numbers" })
vim.keymap.set("n", "<leader>,v", function()
  local current_config = vim.diagnostic.config()
  current_config.virtual_lines = not current_config.virtual_lines
  vim.diagnostic.config(current_config)
  if current_config.virtual_lines then
    print("LSP virtual lines enabled")
  else
    print("LSP virtual lines disabled")
  end
end, { desc = "Toggle LSP virtual lines" })

-- Tab management
vim.keymap.set('n', '<leader>tc', ':tabnew<CR>', { desc = 'Create tab' })
vim.keymap.set('n', '<leader>td', ':tabclose<CR>', { desc = 'Close tab' })
vim.keymap.set('n', '<leader>tn', ':tabnext<CR>', { desc = 'Next tab' })
vim.keymap.set('n', '<leader>tp', ':tabprev<CR>', { desc = 'Prev tab' })

-- Tab moving
vim.keymap.set('n', '<leader>tm', ':tabmove<CR>', { desc = 'Move tab' })
vim.keymap.set('n', '<leader>t>', ':tabmove +1<CR>', { desc = 'Move tab right' })
vim.keymap.set('n', '<leader>t<', ':tabmove -1<CR>', { desc = 'Move tab left' })

-- Enhanced tab keybindings
vim.keymap.set('n', '<leader>tO', tabs.open_file_in_tab, { desc = 'Open file in new tab' })
vim.keymap.set('n', '<leader>tD', tabs.duplicate_tab, { desc = 'Duplicate current tab' })
vim.keymap.set('n', '<leader>tr', tabs.close_tabs_right, { desc = 'Close tabs to the right' })
vim.keymap.set('n', '<leader>tL', tabs.close_tabs_left, { desc = 'Close tabs to the left' })