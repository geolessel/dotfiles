vim.pack.add {
  { src = 'https://github.com/nvim-mini/mini.pick' },
  { src = 'https://github.com/nvim-mini/mini.extra' },
  { src = 'https://github.com/nvim-mini/mini.icons' },
}

require('mini.pick').setup()
require('mini.extra').setup()
require('mini.icons').setup()