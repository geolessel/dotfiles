-- Markdown/document preview plugin
vim.pack.add {
  { src = 'https://github.com/OXY2DEV/markview.nvim' },
}

require('markview').setup({
  preview = {
    -- Use hybrid mode for editing
    modes = { "n", "i", "no", "c" },
    hybrid_modes = { "i" },

    -- Callbacks to refresh on colorscheme changes
    callbacks = {
      on_enable = function(_, win)
        vim.wo[win].conceallevel = 2
        vim.wo[win].concealcursor = "nc"
      end
    }
  }
})
