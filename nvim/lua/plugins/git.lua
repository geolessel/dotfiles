vim.pack.add {
  { src = 'https://github.com/NeogitOrg/neogit' },
  { src = 'https://github.com/nvim-lua/plenary.nvim' },
  { src = 'https://github.com/lewis6991/gitsigns.nvim' },
}

require('neogit').setup({
  graph_style = "unicode",
  process_spinner = true,
  integrations = { mini_pick = true }
})

require('gitsigns').setup({
  attach_to_untracked = true,
  numhl = true
})