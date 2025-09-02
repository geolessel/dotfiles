vim.pack.add {
  { src = 'https://github.com/nvim-mini/mini.surround' },
  { src = 'https://github.com/nvim-mini/mini.pairs' },
  { src = 'https://github.com/nvim-mini/mini.indentscope' },
  { src = 'https://github.com/nvim-mini/mini.clue' },
}

require('mini.surround').setup({})
require('mini.pairs').setup()
require('mini.indentscope').setup({
  -- Whether to first check input line to be a border of adjacent scope.
  -- Use it if you want to place cursor on function header to get scope of its body.
  try_as_border = true,
})

local miniclue = require('mini.clue')
miniclue.setup({
  triggers = {
    { mode = 'n', keys = '<Leader>' },
    { mode = 'x', keys = '<Leader>' },

    -- Built-in completion
    { mode = 'i', keys = '<C-x>' },

    -- `g` key
    { mode = 'n', keys = 'g' },
    { mode = 'x', keys = 'g' },

    -- Marks
    { mode = 'n', keys = "'" },
    { mode = 'n', keys = '`' },
    { mode = 'x', keys = "'" },
    { mode = 'x', keys = '`' },

    -- Registers
    { mode = 'n', keys = '"' },
    { mode = 'x', keys = '"' },
    { mode = 'i', keys = '<C-r>' },
    { mode = 'c', keys = '<C-r>' },

    -- Window commands
    { mode = 'n', keys = '<C-w>' },

    -- `z` key
    { mode = 'n', keys = 'z' },
    { mode = 'x', keys = 'z' },
  },
  clues = {
    -- Enhance this by adding descriptions for <Leader> mapping groups
    miniclue.gen_clues.builtin_completion(),
    miniclue.gen_clues.g(),
    miniclue.gen_clues.marks(),
    miniclue.gen_clues.registers(),
    miniclue.gen_clues.windows(),
    miniclue.gen_clues.z(),
    { mode = 'n', keys = '<Leader>g', desc = '+Git' },
    { mode = 'n', keys = '<Leader>b', desc = '+Buffers' },
    { mode = 'n', keys = '<Leader>t', desc = '+Tabs' },
    { mode = 'n', keys = '<Leader>w', desc = '+Windows' },
    { mode = 'n', keys = '<Leader>,', desc = '+Prefs' },
    { mode = 'n', keys = '<Leader>s', desc = '+Search' }
  },

  window = {
    delay = 0,
    config = {
      width = 'auto'
    },
  },
})

