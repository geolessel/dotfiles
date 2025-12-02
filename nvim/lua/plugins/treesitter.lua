vim.pack.add {
  { src = 'https://github.com/nvim-treesitter/nvim-treesitter' },
}

-- Treesitter configuration
require 'nvim-treesitter.configs'.setup {
  ensure_installed = { "elixir", "heex", "eex", "erlang", "ruby", "lua", "typescript", "tsx", "javascript" },
  auto_install = true,
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  },
  indent = {
    enable = true,
  },
}

-- Configure LFE files to use Erlang parser
vim.treesitter.language.register('erlang', 'lfe')
