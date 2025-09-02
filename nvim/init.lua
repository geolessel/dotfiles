-- Core configuration
require('config.options')
require('config.autocmds')
require('config.statusline')

-- Plugin management and setup
require('plugins.lsp')
require('plugins.completion')
require('plugins.picker')
require('plugins.git')
require('plugins.ui')
require('plugins.treesitter')
require('plugins.formatting')
require('plugins.colorscheme')

-- Keymaps (last to ensure all dependencies are loaded)
require('config.keymaps')

